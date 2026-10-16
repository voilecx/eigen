/*
 * This file is part of Eigen.
 * Copyright (C) 2026 Eigen contributors
 *
 * Portions of this file are derived from the following projects, also licensed under
 * the GNU General Public License version 3:
 *   GrimAC - https://github.com/GrimAnticheat/Grim
 *            Copyright (c) 2021-2026 GrimAC, DefineOutside and contributors
 *   Shard  - https://github.com/KaelusAI/Shard
 *            Copyright (C) 2026 KaelusAI
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

import scala.jdk.CollectionConverters.*

import com.github.retrooper.packetevents.event.{PacketListenerAbstract, PacketListenerPriority, PacketSendEvent}
import com.github.retrooper.packetevents.protocol.entity.pose.EntityPose
import com.github.retrooper.packetevents.protocol.entity.`type`.{EntityType, EntityTypes}
import com.github.retrooper.packetevents.protocol.item.ItemStack
import com.github.retrooper.packetevents.protocol.packettype.PacketType
import com.github.retrooper.packetevents.protocol.player.ClientVersion
import com.github.retrooper.packetevents.protocol.teleport.RelativeFlag
import com.github.retrooper.packetevents.wrapper.play.server.*

import eigen.core.entity.TrackedEntity
import eigen.core.sim.{ItemKind, PendingRotation, PendingTeleport}

/**
 * Server → client packets. Each relevant packet is bracketed with transactions and its effect
 * is scheduled as a task that runs when the client acknowledges it, so the simulation always
 * reflects what the client has actually received. Runs at monitor priority and skips packets
 * another plugin cancelled, since those never reach the client.
 *
 * Bracketing scheme derived from GrimAC and Shard (GPL-3.0).
 */
final class Outbound(players: Players) extends PacketListenerAbstract(PacketListenerPriority.MONITOR):
  import Outbound.*
  import PacketType.Play.Server as S

  override def onPacketSend(event: PacketSendEvent): Unit =
    if event.isCancelled then return
    val tpe = event.getPacketType
    if tpe == PacketType.Login.Server.LOGIN_SUCCESS then
      val user = event.getUser
      event.getTasksAfterSend.add(() => { val _ = players.connect(user) })
      return
    val p = players.get(event.getUser)
    if p == null then return

    if tpe == S.PING then p.clock.onPingSent(WrapperPlayServerPing(event).getId)
    else if tpe == S.SPAWN_ENTITY then
      val w = WrapperPlayServerSpawnEntity(event)
      spawn(p, w.getEntityId, w.getEntityType, w.getPosition.getX, w.getPosition.getY, w.getPosition.getZ)
    else if tpe == S.SPAWN_LIVING_ENTITY then
      val w = WrapperPlayServerSpawnLivingEntity(event)
      spawn(p, w.getEntityId, w.getEntityType, w.getPosition.getX, w.getPosition.getY, w.getPosition.getZ)
    else if tpe == S.SPAWN_PLAYER then
      val w = WrapperPlayServerSpawnPlayer(event)
      spawn(p, w.getEntityId, EntityTypes.PLAYER, w.getPosition.getX, w.getPosition.getY, w.getPosition.getZ)
    else if tpe == S.DESTROY_ENTITIES then destroy(p, WrapperPlayServerDestroyEntities(event).getEntityIds)
    else if tpe == S.ENTITY_RELATIVE_MOVE then
      val w = WrapperPlayServerEntityRelativeMove(event)
      move(p, w.getEntityId, relative = true, hasPos = true, w.getDeltaX, w.getDeltaY, w.getDeltaZ)
    else if tpe == S.ENTITY_RELATIVE_MOVE_AND_ROTATION then
      val w = WrapperPlayServerEntityRelativeMoveAndRotation(event)
      move(p, w.getEntityId, relative = true, hasPos = true, w.getDeltaX, w.getDeltaY, w.getDeltaZ)
    else if tpe == S.ENTITY_TELEPORT then
      val w = WrapperPlayServerEntityTeleport(event)
      val pos = w.getValues.getPosition
      move(p, w.getEntityId, relative = false, hasPos = true, pos.getX, pos.getY, pos.getZ)
    else if tpe == S.ENTITY_POSITION_SYNC then
      val w = WrapperPlayServerEntityPositionSync(event)
      val pos = w.getValues.getPosition
      move(p, w.getId, relative = false, hasPos = true, pos.getX, pos.getY, pos.getZ)
    else if tpe == S.ENTITY_ROTATION then move(p, WrapperPlayServerEntityRotation(event).getEntityId, relative = true, hasPos = false, 0, 0, 0)
    else if tpe == S.ENTITY_STATUS then
      val w = WrapperPlayServerEntityStatus(event)
      if w.getStatus == DeathStatus then
        val id = w.getEntityId
        p.clock.at(p.bracket(event))(() => { val e = p.sim.entity(id); if e != null then e.dead = true })
    else if tpe == S.ENTITY_METADATA then metadata(p, WrapperPlayServerEntityMetadata(event))
    else if tpe == S.UPDATE_ATTRIBUTES then attributes(p, event, WrapperPlayServerUpdateAttributes(event))
    else if tpe == S.ENTITY_VELOCITY then
      if WrapperPlayServerEntityVelocity(event).getEntityId == p.sim.selfId then p.clock.at(p.bracket(event))(() => p.sim.onKnockback())
    else if tpe == S.EXPLOSION then
      val motion = WrapperPlayServerExplosion(event).getKnockback
      if motion != null && (motion.getX != 0 || motion.getY != 0 || motion.getZ != 0) then
        p.clock.at(p.bracket(event))(() => p.sim.onKnockback())
    else if tpe == S.PLAYER_POSITION_AND_LOOK then
      val w = WrapperPlayServerPlayerPositionAndLook(event)
      val f = w.getRelativeFlags
      val teleport = PendingTeleport(
        w.getX, w.getY, w.getZ, w.getYaw, w.getPitch,
        f.has(RelativeFlag.X), f.has(RelativeFlag.Y), f.has(RelativeFlag.Z), f.has(RelativeFlag.YAW), f.has(RelativeFlag.PITCH),
      )
      p.clock.at(p.bracket(event))(() => p.sim.expectTeleport(teleport))
    else if tpe == S.PLAYER_ROTATION then
      val w = WrapperPlayServerPlayerRotation(event)
      val pitch = if w.isRelativePitch then w.getPitch else math.max(-90f, math.min(90f, w.getPitch % 360f))
      val rotation = PendingRotation(w.getYaw, pitch, w.isRelativeYaw, w.isRelativePitch)
      p.sendTransaction()
      p.clock.at(p.clock.lastSent)(() => p.sim.expectRotation(rotation))
    else if tpe == S.JOIN_GAME then
      val w = WrapperPlayServerJoinGame(event)
      val self = w.getEntityId
      p.clock.at(p.clock.lastSent)(() => { p.sim.breakEpoch(); p.sim.selfId = self; p.entityMeta.clear() })
      event.getTasksAfterSend.add(() => p.sendTransaction())
    else if tpe == S.RESPAWN then
      p.clock.at(p.bracket(event))(() => { p.sim.breakEpoch(); p.entityMeta.clear() })
    else if tpe == S.SET_PASSENGERS then
      val w = WrapperPlayServerSetPassengers(event)
      val vehicle = w.getEntityId
      val riding = w.getPassengers.contains(p.sim.selfId)
      p.clock.at(p.bracket(event))(() => vehicleChange(p, vehicle, riding))
    else if tpe == S.OPEN_WINDOW then p.clock.at(p.clock.lastSent)(() => p.sim.onGui(true))
    else if tpe == S.CLOSE_WINDOW then p.clock.at(p.clock.lastSent)(() => p.sim.onGui(false))
    else if tpe == S.HELD_ITEM_CHANGE then
      val slot = WrapperPlayServerHeldItemChange(event).getSlot
      p.clock.at(p.clock.lastSent)(() => p.sim.onSlot(slot))
    else if tpe == S.SET_SLOT then
      val w = WrapperPlayServerSetSlot(event)
      if w.getWindowId == 0 then inventorySlot(p, w.getSlot, w.getItem)
    else if tpe == S.WINDOW_ITEMS then
      val w = WrapperPlayServerWindowItems(event)
      if w.getWindowId == 0 then
        val items = w.getItems.asScala.toIndexedSeq
        for i <- 36 to 45 if i < items.size do inventorySlot(p, i, items(i))
    else if tpe == S.SET_PLAYER_INVENTORY then
      val w = WrapperPlayServerSetPlayerInventory(event)
      // Player inventory indices: 0..8 hotbar, 40 offhand.
      val slot = w.getSlot
      if slot >= 0 && slot < 9 then inventorySlot(p, 36 + slot, w.getStack)
      else if slot == 40 then inventorySlot(p, 45, w.getStack)

  private def spawn(p: EigenPlayer, id: Int, tpe: EntityType, x: Double, y: Double, z: Double): Unit =
    if id < 0 then return
    p.forgetEntity(id)
    val modernBoats = p.user.getClientVersion.isNewerThanOrEquals(ClientVersion.V_1_21_2)
    val e = TrackedEntity(id, EntityCatalog.kind(tpe), EntityCatalog.steps(tpe, modernBoats), p.profile)
    e.spawnAt(x, y, z)
    val meta = EntityMeta(tpe)
    EntityCatalog.resize(e, tpe, p.profile, crouching = false, lowPose = false, scale = 1f)
    p.clock.at(p.clock.lastSent)(() => { p.entityMeta(id) = meta; p.sim.addEntity(e) })

  private def destroy(p: EigenPlayer, ids: Array[Int]): Unit =
    val t = p.clock.lastSent
    p.clock.at(t): () =>
      ids.foreach: id =>
        val e = p.sim.entity(id)
        if e != null then e.dead = true
    p.clock.at(t + 1): () =>
      ids.foreach: id =>
        p.sim.removeEntity(id)
        p.entityMeta.remove(id)
        p.forgetEntity(id)

  private def move(p: EigenPlayer, id: Int, relative: Boolean, hasPos: Boolean, x: Double, y: Double, z: Double): Unit =
    if id == p.sim.selfId then return
    val t = p.movementTransaction(id)
    p.clock.at(t): () =>
      val e = p.sim.entity(id)
      if e != null then e.onFirstTransaction(relative, hasPos, x, y, z)
    p.clock.at(t + 1): () =>
      val e = p.sim.entity(id)
      if e != null then e.onSecondTransaction()

  private def metadata(p: EigenPlayer, w: WrapperPlayServerEntityMetadata): Unit =
    val id = w.getEntityId
    val self = id == p.sim.selfId
    val data = w.getEntityMetadata.asScala.toSeq
    p.clock.at(p.clock.lastSent): () =>
      data.foreach: d =>
        (d.getIndex, d.getValue) match
          case (0, flags: java.lang.Byte) if self =>
            p.sim.onSelfFlags(gliding = (flags & 0x80) != 0, swimming = (flags & 0x10) != 0)
          case (LivingFlagsIndex, flags: java.lang.Byte) if self =>
            p.sim.onSelfLivingFlags(riptide = (flags & 0x04) != 0)
          case (_, pose: EntityPose) if !self =>
            val meta = p.entityMeta.getOrNull(id)
            val e = p.sim.entity(id)
            if meta != null && e != null then
              meta.crouching = pose == EntityPose.CROUCHING
              meta.lowPose = pose == EntityPose.SWIMMING || pose == EntityPose.FALL_FLYING || pose == EntityPose.SPIN_ATTACK
              EntityCatalog.resize(e, meta.tpe, p.profile, meta.crouching, meta.lowPose, meta.scale)
          case _ => ()

  private def attributes(p: EigenPlayer, event: PacketSendEvent, w: WrapperPlayServerUpdateAttributes): Unit =
    val id = w.getEntityId
    val self = id == p.sim.selfId
    val values = w.getProperties.asScala.toSeq.map(prop => prop.getAttribute.getName.getKey.toLowerCase -> prop.calcValue())
    val t = if self then p.bracket(event) else p.clock.lastSent
    p.clock.at(t): () =>
      values.foreach: (key, value) =>
        if key.endsWith("scale") then
          if self then p.sim.onSelfScale(value.toFloat)
          else
            val meta = p.entityMeta.getOrNull(id)
            val e = p.sim.entity(id)
            if meta != null && e != null then
              meta.scale = value.toFloat
              EntityCatalog.resize(e, meta.tpe, p.profile, meta.crouching, meta.lowPose, meta.scale)
        else if self && key.endsWith("attack_speed") then p.sim.onAttackSpeed(value)
        else if self && key.endsWith("entity_interaction_range") then p.sim.onInteractionRange(value)

  private def vehicleChange(p: EigenPlayer, vehicle: Int, riding: Boolean): Unit =
    if riding then p.sim.onVehicle(true)
    else if p.sim.body.inVehicle && p.sim.entity(vehicle) != null then p.sim.onVehicle(false)

  /** Window-0 slot indices: 36..44 hotbar, 45 offhand. */
  private def inventorySlot(p: EigenPlayer, slot: Int, item: ItemStack): Unit =
    val kind = if item == null || item.isEmpty then ItemKind.Empty else ItemKind.of(item.getType.getName.toString)
    p.clock.at(p.clock.lastSent): () =>
      if slot >= 36 && slot <= 44 then p.sim.inventory.hotbar(slot - 36) = kind
      else if slot == 45 then p.sim.inventory.offhand = kind

object Outbound:
  private inline val DeathStatus = 3
  /** Index of the living-entity flags byte in entity metadata (1.17+). */
  private inline val LivingFlagsIndex = 8
