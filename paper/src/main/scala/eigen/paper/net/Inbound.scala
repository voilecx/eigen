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
import com.github.retrooper.packetevents.protocol.packettype.{PacketType, PacketTypeCommon}
import com.github.retrooper.packetevents.protocol.player.{DiggingAction, InteractionHand}
import com.github.retrooper.packetevents.wrapper.configuration.client.WrapperConfigClientPluginMessage
import com.github.retrooper.packetevents.wrapper.play.client.*

import eigen.paper.collect.Collector

/**
 * Client → server packets. Runs on the connection's network thread at the lowest priority, so
 * it sees every packet the client sent even if another plugin later cancels it. Only our own
 * transaction answers are cancelled.
 */
final class Inbound(players: Players, collector: Collector) extends PacketListenerAbstract(PacketListenerPriority.LOWEST):
  import PacketType.Play.Client as C

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
        route(p, event, tpe)

  private def route(p: EigenPlayer, event: PacketReceiveEvent, tpe: PacketTypeCommon): Unit = tpe match
    case C.PONG =>
      if p.clock.onPong(WrapperPlayClientPong(event).getId) then event.setCancelled(true)
    case C.CLIENT_TICK_END =>
      prepareTick(p)
      p.sim.onTickEnd()
    case C.PLAYER_INPUT =>
      val i = WrapperPlayClientPlayerInput(event)
      p.sim.onInput(i.isForward, i.isBackward, i.isLeft, i.isRight, i.isJump, i.isShift, i.isSprint)
    case C.INTERACT_ENTITY =>
      val i = WrapperPlayClientInteractEntity(event)
      if i.getAction == WrapperPlayClientInteractEntity.InteractAction.ATTACK then p.sim.onAttack(i.getEntityId)
    case C.ATTACK => p.sim.onAttack(WrapperPlayClientAttack(event).getEntityId)
    case C.ANIMATION =>
      if WrapperPlayClientAnimation(event).getHand == InteractionHand.MAIN_HAND then p.sim.onSwing()
    case C.USE_ITEM => p.sim.onUse()
    case C.PLAYER_BLOCK_PLACEMENT => p.sim.onPlace()
    case C.PLAYER_DIGGING =>
      if WrapperPlayClientPlayerDigging(event).getAction == DiggingAction.START_DIGGING then p.sim.onDig()
    case C.HELD_ITEM_CHANGE => p.sim.onSlot(WrapperPlayClientHeldItemChange(event).getSlot)
    case C.ENTITY_ACTION => entityAction(p, WrapperPlayClientEntityAction(event).getAction)
    case C.CLICK_WINDOW => p.sim.onGui(true)
    case C.CLOSE_WINDOW => p.sim.onGui(false)
    case C.PLUGIN_MESSAGE =>
      val m = WrapperPlayClientPluginMessage(event)
      brand(event, m.getChannelName, m.getData)
    case _ if WrapperPlayClientPlayerFlying.isFlying(tpe) =>
      val f = WrapperPlayClientPlayerFlying(event)
      val loc = f.getLocation
      prepareTick(p)
      p.sim.onFlying(f.hasPositionChanged, loc.getX, loc.getY, loc.getZ, f.hasRotationChanged, loc.getYaw, loc.getPitch, f.isOnGround)
    case _ => ()

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
