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
      brand(event, WrapperConfigClientPluginMessage(event).getChannelName, WrapperConfigClientPluginMessage(event).getData)
      return
    val p = players.get(event.getUser)
    if p == null then return
    p.drain()
    val sim = p.sim

    if tpe == C.PONG then
      if p.clock.onPong(WrapperPlayClientPong(event).getId) then event.setCancelled(true)
    else if WrapperPlayClientPlayerFlying.isFlying(tpe) then
      val f = WrapperPlayClientPlayerFlying(event)
      val loc = f.getLocation
      prepareTick(p)
      sim.onFlying(f.hasPositionChanged, loc.getX, loc.getY, loc.getZ, f.hasRotationChanged, loc.getYaw, loc.getPitch, f.isOnGround)
    else if tpe == C.CLIENT_TICK_END then
      prepareTick(p)
      sim.onTickEnd()
    else if tpe == C.PLAYER_INPUT then
      val i = WrapperPlayClientPlayerInput(event)
      sim.onInput(i.isForward, i.isBackward, i.isLeft, i.isRight, i.isJump, i.isShift, i.isSprint)
    else if tpe == C.INTERACT_ENTITY then
      val i = WrapperPlayClientInteractEntity(event)
      if i.getAction == WrapperPlayClientInteractEntity.InteractAction.ATTACK then sim.onAttack(i.getEntityId)
    else if tpe == C.ATTACK then sim.onAttack(WrapperPlayClientAttack(event).getEntityId)
    else if tpe == C.ANIMATION then
      if WrapperPlayClientAnimation(event).getHand == InteractionHand.MAIN_HAND then sim.onSwing()
    else if tpe == C.USE_ITEM then sim.onUse()
    else if tpe == C.PLAYER_BLOCK_PLACEMENT then sim.onPlace()
    else if tpe == C.PLAYER_DIGGING then
      if WrapperPlayClientPlayerDigging(event).getAction == DiggingAction.START_DIGGING then sim.onDig()
    else if tpe == C.HELD_ITEM_CHANGE then sim.onSlot(WrapperPlayClientHeldItemChange(event).getSlot)
    else if tpe == C.ENTITY_ACTION then
      import WrapperPlayClientEntityAction.Action
      WrapperPlayClientEntityAction(event).getAction match
        case Action.START_SPRINTING => sim.onSprint(true)
        case Action.STOP_SPRINTING => sim.onSprint(false)
        case Action.START_SNEAKING => sim.onSneak(true)
        case Action.STOP_SNEAKING => sim.onSneak(false)
        case _ => ()
    else if tpe == C.CLICK_WINDOW then sim.onGui(true)
    else if tpe == C.CLOSE_WINDOW then sim.onGui(false)
    else if tpe == C.PLUGIN_MESSAGE then
      val m = WrapperPlayClientPluginMessage(event)
      brand(event, m.getChannelName, m.getData)

  /** Per-tick inputs the simulation reads from outside. */
  private def prepareTick(p: EigenPlayer): Unit =
    p.sim.windowsWanted = players.wantsWindows(p)
    p.sim.pingBucket = p.pingBucket

  private def brand(event: PacketReceiveEvent, channel: String, data: Array[Byte]): Unit =
    if (channel == "minecraft:brand" || channel == "MC|Brand") && data.length > 1 && data.length < 256 then
      // A length-prefixed string; brands are short, so the prefix is a single byte.
      val text = String(data, 1, data.length - 1, UTF_8).replace(" (Velocity)", "")
      players.rememberBrand(event.getUser, text.take(64))
