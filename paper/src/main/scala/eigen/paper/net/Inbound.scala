/*
 * This file is part of Eigen.
 * Copyright (C) 2026 Eigen contributors
 *
 * Eigen is free software: you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation, version 3.
 *
 * Eigen is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Eigen. If not,
 * see <https://www.gnu.org/licenses/>.
 */

package eigen.paper.net

import java.nio.charset.StandardCharsets.UTF_8

import com.github.retrooper.packetevents.event.{PacketListenerAbstract, PacketListenerPriority, PacketReceiveEvent, UserDisconnectEvent, UserLoginEvent}
import com.github.retrooper.packetevents.protocol.packettype.PacketType
import com.github.retrooper.packetevents.protocol.player.{DiggingAction, InteractionHand}
import com.github.retrooper.packetevents.wrapper.configuration.client.WrapperConfigClientPluginMessage
import com.github.retrooper.packetevents.wrapper.play.client.*

import eigen.paper.collect.Collector

/**
 * Client → server packets. Runs on the connection's network thread at the lowest priority, so
 * it sees every packet the client sent even if another plugin later cancels it. Only our own
 * transaction answers are cancelled.
 *
 * Dispatch is an O(1) array lookup indexed by `PacketType.Play.Client.ordinal` via [[PacketTable]],
 * with the server-version-specific transaction packet (`PONG` on 1.17+, `WINDOW_CONFIRMATION` on
 * `< 1.17`) bound once at construction time through [[ServerProtocol]].
 */
final class Inbound(players: Players, collector: Collector) extends PacketListenerAbstract(PacketListenerPriority.LOWEST):
  import Inbound.Handler
  import PacketType.Play.Client as C

  private val protocol: ServerProtocol = ServerProtocol.current
  private val flyingHandler: Handler = (p, event) =>
    val f = WrapperPlayClientPlayerFlying(event)
    val loc = f.getLocation
    prepareTick(p)
    p.sim.onFlying(f.hasPositionChanged, loc.getX, loc.getY, loc.getZ, f.hasRotationChanged, loc.getYaw, loc.getPitch, f.isOnGround)

  private val table: PacketTable[C, Handler] = PacketTable.build(C.values)(
    protocol.inboundTransactionType -> ((p, event) =>
      val id = protocol.readAckId(event)
      if id != 0 && p.clock.onPong(id) then event.setCancelled(true)
    ),
    C.CLIENT_TICK_END -> ((p, _) => { prepareTick(p); p.sim.onTickEnd() }),
    C.PLAYER_INPUT -> ((p, event) =>
      val i = WrapperPlayClientPlayerInput(event)
      p.sim.onInput(i.isForward, i.isBackward, i.isLeft, i.isRight, i.isJump, i.isShift, i.isSprint)
    ),
    C.INTERACT_ENTITY -> ((p, event) =>
      val i = WrapperPlayClientInteractEntity(event)
      if i.getAction == WrapperPlayClientInteractEntity.InteractAction.ATTACK then p.sim.onAttack(i.getEntityId)
    ),
    C.ATTACK -> ((p, event) => p.sim.onAttack(WrapperPlayClientAttack(event).getEntityId)),
    C.ANIMATION -> ((p, event) =>
      if WrapperPlayClientAnimation(event).getHand == InteractionHand.MAIN_HAND then p.sim.onSwing()
    ),
    C.USE_ITEM -> ((p, _) => p.sim.onUse()),
    C.PLAYER_BLOCK_PLACEMENT -> ((p, _) => p.sim.onPlace()),
    C.PLAYER_DIGGING -> ((p, event) =>
      if WrapperPlayClientPlayerDigging(event).getAction == DiggingAction.START_DIGGING then p.sim.onDig()
    ),
    C.HELD_ITEM_CHANGE -> ((p, event) => p.sim.onSlot(WrapperPlayClientHeldItemChange(event).getSlot)),
    C.ENTITY_ACTION -> ((p, event) => entityAction(p, WrapperPlayClientEntityAction(event).getAction)),
    C.CLICK_WINDOW -> ((p, _) => p.sim.onGui(true)),
    C.CLOSE_WINDOW -> ((p, _) => p.sim.onGui(false)),
    C.PLUGIN_MESSAGE -> ((_, event) =>
      val m = WrapperPlayClientPluginMessage(event)
      brand(event, m.getChannelName, m.getData)
    ),
    C.PLAYER_FLYING -> flyingHandler,
    C.PLAYER_POSITION -> flyingHandler,
    C.PLAYER_ROTATION -> flyingHandler,
    C.PLAYER_POSITION_AND_ROTATION -> flyingHandler,
  )

  override def onUserLogin(event: UserLoginEvent): Unit =
    val p = players.get(event.getUser)
    if p != null then
      val bukkit: org.bukkit.entity.Player = event.getPlayer
      players.announce(p, if bukkit != null then bukkit.getName else event.getUser.getName)

  override def onUserDisconnect(event: UserDisconnectEvent): Unit =
    // Finish any recording while the player is still tracked; the disconnect removes it.
    collector.onQuit(event.getUser.getUUID)
    val _ = players.disconnect(event.getUser)

  override def onPacketReceive(event: PacketReceiveEvent): Unit =
    val tpe = event.getPacketType
    if tpe == PacketType.Configuration.Client.PLUGIN_MESSAGE then
      val message = WrapperConfigClientPluginMessage(event)
      brand(event, message.getChannelName, message.getData)
    else
      val p = players.get(event.getUser)
      if p != null then
        p.drain()
        val h = table.get(tpe)
        if h != null then h.handle(p, event)

  private def entityAction(p: EigenPlayer, action: WrapperPlayClientEntityAction.Action): Unit =
    import WrapperPlayClientEntityAction.Action
    action match
      case Action.START_SPRINTING => p.sim.onSprint(true)
      case Action.STOP_SPRINTING => p.sim.onSprint(false)
      case Action.START_SNEAKING => p.sim.onSneak(true)
      case Action.STOP_SNEAKING => p.sim.onSneak(false)
      case _ => ()

  /** Per-tick inputs the simulation reads from outside. */
  private def prepareTick(p: EigenPlayer): Unit =
    p.sim.windowsWanted = players.wantsWindows(p)
    p.sim.pingBucket = p.pingBucket

  /** A brand is a length-prefixed string; brands are short, so the prefix is a single byte. */
  private def brand(event: PacketReceiveEvent, channel: String, data: Array[Byte]): Unit =
    if (channel == "minecraft:brand" || channel == "MC|Brand") && data.length > 1 && data.length < 256 then
      val text = String(data, 1, data.length - 1, UTF_8).replace(" (Velocity)", "")
      players.rememberBrand(event.getUser, text.take(64))

object Inbound:
  @FunctionalInterface
  trait Handler:
    def handle(p: EigenPlayer, event: PacketReceiveEvent): Unit
