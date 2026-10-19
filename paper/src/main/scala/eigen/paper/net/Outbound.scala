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

import com.github.retrooper.packetevents.PacketEvents
import com.github.retrooper.packetevents.event.{PacketListenerAbstract, PacketListenerPriority, PacketSendEvent}
import com.github.retrooper.packetevents.manager.server.ServerVersion
import com.github.retrooper.packetevents.protocol.entity.data.EntityData
import com.github.retrooper.packetevents.protocol.entity.pose.EntityPose
import com.github.retrooper.packetevents.protocol.entity.`type`.{EntityType, EntityTypes}
import com.github.retrooper.packetevents.protocol.item.ItemStack
import com.github.retrooper.packetevents.protocol.packettype.{PacketType, PacketTypeCommon}
import com.github.retrooper.packetevents.protocol.player.ClientVersion
import com.github.retrooper.packetevents.protocol.teleport.RelativeFlag
import com.github.retrooper.packetevents.util.Vector3d
import com.github.retrooper.packetevents.wrapper.play.server.*

import eigen.core.entity.TrackedEntity
import eigen.core.sim.{ItemKind, PendingRotation, PendingTeleport}

/**
 * Server → client packets. Each relevant packet is bracketed with transactions and its effect
 * is scheduled as a task that runs when the client acknowledges it, so the simulation always
 * reflects what the client has actually received. Runs at monitor priority and skips packets
 * another plugin cancelled, since those never reach the client.
 */
final class Outbound(players: Players) extends PacketListenerAbstract(PacketListenerPriority.MONITOR):
  import Outbound.*
  import PacketType.Play.Server as S

  // Fixed per process; metadata indices follow the server version.
  private val livingIdx = livingFlagsIndex(PacketEvents.getAPI.getServerManager.getVersion)

  override def onPacketSend(event: PacketSendEvent): Unit =
    if event.isCancelled then return
    val tpe = event.getPacketType
    if tpe == PacketType.Login.Server.LOGIN_SUCCESS then
      val user = event.getUser
      event.getTasksAfterSend.add(() => { val _ = players.connect(user) })
    else
      val p = players.get(event.getUser)
      if p != null then route(p, event, tpe)

  private def route(p: EigenPlayer, event: PacketSendEvent, tpe: PacketTypeCommon): Unit = tpe match
    case S.PING => p.clock.onPingSent(WrapperPlayServerPing(event).getId)
    case S.SPAWN_ENTITY =>
      val w = WrapperPlayServerSpawnEntity(event)
      spawn(p, w.getEntityId, w.getEntityType, w.getPosition)
    case S.SPAWN_LIVING_ENTITY =>
      val w = WrapperPlayServerSpawnLivingEntity(event)
      spawn(p, w.getEntityId, w.getEntityType, w.getPosition)
    case S.SPAWN_PLAYER =>
      val w = WrapperPlayServerSpawnPlayer(event)
      spawn(p, w.getEntityId, EntityTypes.PLAYER, w.getPosition)
    case S.DESTROY_ENTITIES => destroy(p, WrapperPlayServerDestroyEntities(event).getEntityIds)
    case S.ENTITY_RELATIVE_MOVE =>
      val w = WrapperPlayServerEntityRelativeMove(event)
      move(p, w.getEntityId, relative = true, hasPos = true, w.getDeltaX, w.getDeltaY, w.getDeltaZ)
    case S.ENTITY_RELATIVE_MOVE_AND_ROTATION =>
      val w = WrapperPlayServerEntityRelativeMoveAndRotation(event)
      move(p, w.getEntityId, relative = true, hasPos = true, w.getDeltaX, w.getDeltaY, w.getDeltaZ)
    case S.ENTITY_TELEPORT =>
      val w = WrapperPlayServerEntityTeleport(event)
      absolute(p, w.getEntityId, w.getValues.getPosition)
    case S.ENTITY_POSITION_SYNC =>
      val w = WrapperPlayServerEntityPositionSync(event)
      absolute(p, w.getId, w.getValues.getPosition)
    case S.ENTITY_ROTATION =>
      move(p, WrapperPlayServerEntityRotation(event).getEntityId, relative = true, hasPos = false, 0, 0, 0)
    case S.ENTITY_STATUS => deathStatus(p, event, WrapperPlayServerEntityStatus(event))
    case S.ENTITY_METADATA => metadata(p, WrapperPlayServerEntityMetadata(event))
    case S.UPDATE_ATTRIBUTES => attributes(p, event, WrapperPlayServerUpdateAttributes(event))
    case S.ENTITY_VELOCITY =>
      if WrapperPlayServerEntityVelocity(event).getEntityId == p.sim.selfId then knockback(p, event)
    case S.EXPLOSION =>
      val motion = WrapperPlayServerExplosion(event).getKnockback
      if motion != null && (motion.getX != 0 || motion.getY != 0 || motion.getZ != 0) then knockback(p, event)
    case S.PLAYER_POSITION_AND_LOOK => teleport(p, event, WrapperPlayServerPlayerPositionAndLook(event))
    case S.PLAYER_ROTATION => serverRotation(p, WrapperPlayServerPlayerRotation(event))
    case S.JOIN_GAME => joinGame(p, event, WrapperPlayServerJoinGame(event).getEntityId)
    case S.RESPAWN =>
      p.clock.at(p.bracket(event))(() => { p.sim.breakEpoch(); p.entityMeta.clear() })
    case S.SET_PASSENGERS =>
      val w = WrapperPlayServerSetPassengers(event)
      val vehicle = w.getEntityId
      val riding = w.getPassengers.contains(p.sim.selfId)
      p.clock.at(p.bracket(event))(() => vehicleChange(p, vehicle, riding))
    case S.OPEN_WINDOW => p.clock.at(p.clock.lastSent)(() => p.sim.onGui(true))
    case S.CLOSE_WINDOW => p.clock.at(p.clock.lastSent)(() => p.sim.onGui(false))
    case S.HELD_ITEM_CHANGE =>
      val slot = WrapperPlayServerHeldItemChange(event).getSlot
      p.clock.at(p.clock.lastSent)(() => p.sim.onSlot(slot))
    case S.SET_SLOT =>
      val w = WrapperPlayServerSetSlot(event)
      if w.getWindowId == 0 then inventorySlot(p, w.getSlot, w.getItem)
    case S.WINDOW_ITEMS =>
      val w = WrapperPlayServerWindowItems(event)
      if w.getWindowId == 0 then
        val items = w.getItems.asScala.toIndexedSeq
        for slot <- FirstHotbarSlot to OffhandSlot if slot < items.size do inventorySlot(p, slot, items(slot))
    case S.SET_PLAYER_INVENTORY =>
      val w = WrapperPlayServerSetPlayerInventory(event)
      // Player inventory indices: 0..8 hotbar, 40 offhand.
      val slot = w.getSlot
      if slot >= 0 && slot < 9 then inventorySlot(p, FirstHotbarSlot + slot, w.getStack)
      else if slot == PlayerOffhandIndex then inventorySlot(p, OffhandSlot, w.getStack)
    case _ => ()

  private def deathStatus(p: EigenPlayer, event: PacketSendEvent, w: WrapperPlayServerEntityStatus): Unit =
    if w.getStatus == DeathStatus then
      val id = w.getEntityId
      p.clock.at(p.bracket(event)): () =>
        val e = p.sim.entity(id)
        if e != null then e.dead = true

  private def knockback(p: EigenPlayer, event: PacketSendEvent): Unit =
    p.clock.at(p.bracket(event))(() => p.sim.onKnockback())

  private def teleport(p: EigenPlayer, event: PacketSendEvent, w: WrapperPlayServerPlayerPositionAndLook): Unit =
    val f = w.getRelativeFlags
    val pending = PendingTeleport(
      w.getX, w.getY, w.getZ, w.getYaw, w.getPitch,
      f.has(RelativeFlag.X), f.has(RelativeFlag.Y), f.has(RelativeFlag.Z), f.has(RelativeFlag.YAW), f.has(RelativeFlag.PITCH),
    )
    p.clock.at(p.bracket(event))(() => p.sim.expectTeleport(pending))

  private def serverRotation(p: EigenPlayer, w: WrapperPlayServerPlayerRotation): Unit =
    val pitch = if w.isRelativePitch then w.getPitch else math.max(-90f, math.min(90f, w.getPitch % 360f))
    val rotation = PendingRotation(w.getYaw, pitch, w.isRelativeYaw, w.isRelativePitch)
    p.sendTransaction()
    p.clock.at(p.clock.lastSent)(() => p.sim.expectRotation(rotation))

  private def joinGame(p: EigenPlayer, event: PacketSendEvent, self: Int): Unit =
    p.clock.at(p.clock.lastSent)(() => { p.sim.breakEpoch(); p.sim.selfId = self; p.entityMeta.clear() })
    event.getTasksAfterSend.add(() => p.sendTransaction())

  private def spawn(p: EigenPlayer, id: Int, tpe: EntityType, pos: Vector3d): Unit =
    if id < 0 then return
    p.forgetEntity(id)
    val modernBoats = p.user.getClientVersion.isNewerThanOrEquals(ClientVersion.V_1_21_2)
    val e = TrackedEntity(id, EntityCatalog.kind(tpe), EntityCatalog.steps(tpe, modernBoats), p.profile)
    e.spawnAt(pos.getX, pos.getY, pos.getZ)
    val meta = EntityMeta(tpe)
    EntityCatalog.resize(e, tpe, p.profile, crouching = false, lowPose = false, scale = 1f)
    p.clock.at(p.clock.lastSent)(() => { p.entityMeta(id) = meta; p.sim.addEntity(e) })

  private def destroy(p: EigenPlayer, ids: Array[Int]): Unit =
    val t = p.clock.lastSent
    // Transaction bookkeeping is touched only from the sending thread.
    ids.foreach(p.forgetEntity)
    p.clock.at(t): () =>
      ids.foreach: id =>
        val e = p.sim.entity(id)
        if e != null then e.dead = true
    p.clock.at(t + 1): () =>
      ids.foreach: id =>
        p.sim.removeEntity(id)
        p.entityMeta.remove(id)

  private def absolute(p: EigenPlayer, id: Int, pos: Vector3d): Unit =
    move(p, id, relative = false, hasPos = true, pos.getX, pos.getY, pos.getZ)

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
    val entries = w.getEntityMetadata
    val n = entries.size
    // Almost every metadata packet carries nothing the simulation tracks; filtering before
    // scheduling keeps those packets free of collection conversion, task and closure.
    var i = 0
    var relevant = false
    while !relevant && i < n do
      relevant = isRelevantMeta(self, entries.get(i))
      i += 1
    if !relevant then return
    val kept = new java.util.ArrayList[EntityData[?]](2)
    i = 0
    while i < n do
      val d = entries.get(i)
      if isRelevantMeta(self, d) then kept.add(d)
      i += 1
    p.clock.at(p.clock.lastSent): () =>
      var j = 0
      while j < kept.size do
        applyMeta(p, id, self, kept.get(j))
        j += 1

  private def isRelevantMeta(self: Boolean, d: EntityData[?]): Boolean =
    val v = d.getValue
    if v.isInstanceOf[java.lang.Byte] then self && (d.getIndex == 0 || d.getIndex == livingIdx)
    else v.isInstanceOf[EntityPose] && !self

  private def applyMeta(p: EigenPlayer, id: Int, self: Boolean, d: EntityData[?]): Unit =
    (d.getIndex, d.getValue) match
      case (0, flags: java.lang.Byte) if self =>
        p.sim.onSelfFlags(gliding = (flags & 0x80) != 0, swimming = (flags & 0x10) != 0)
      case (i, flags: java.lang.Byte) if self && i == livingIdx =>
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
    val props = w.getProperties
    val n = props.size
    // Most attribute packets (armour, luck, ...) touch nothing tracked here; skipping them
    // also skips the bracket transactions a self packet would otherwise cost.
    var i = 0
    var relevant = false
    while !relevant && i < n do
      relevant = isRelevantAttribute(self, props.get(i).getAttribute.getName.getKey)
      i += 1
    if !relevant then return
    val values = new java.util.ArrayList[(String, Double)](2)
    i = 0
    while i < n do
      val prop = props.get(i)
      val key = prop.getAttribute.getName.getKey
      if isRelevantAttribute(self, key) then values.add((key, prop.calcValue()))
      i += 1
    val t = if self then p.bracket(event) else p.clock.lastSent
    p.clock.at(t): () =>
      var j = 0
      while j < values.size do
        val (key, value) = values.get(j)
        applyAttribute(p, id, self, key, value)
        j += 1

  private def isRelevantAttribute(self: Boolean, key: String): Boolean =
    key.endsWith("scale") || (self && (key.endsWith("attack_speed") || key.endsWith("entity_interaction_range")))

  private def applyAttribute(p: EigenPlayer, id: Int, self: Boolean, key: String, value: Double): Unit =
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

  /** Stores an item in the simulation's inventory mirror; `slot` is a window-0 slot index. */
  private def inventorySlot(p: EigenPlayer, slot: Int, item: ItemStack): Unit =
    val kind = if item == null || item.isEmpty then ItemKind.Empty else ItemKind.of(item.getType.getName.toString)
    p.clock.at(p.clock.lastSent): () =>
      if slot >= FirstHotbarSlot && slot < OffhandSlot then p.sim.inventory.hotbar(slot - FirstHotbarSlot) = kind
      else if slot == OffhandSlot then p.sim.inventory.offhand = kind

object Outbound:
  private inline val DeathStatus = 3
  /** Window-0 slot of the first hotbar slot; the hotbar spans nine slots. */
  private inline val FirstHotbarSlot = 36
  /** Window-0 offhand slot. */
  private inline val OffhandSlot = 45
  /** Player-inventory index of the offhand, as used by `SET_PLAYER_INVENTORY`. */
  private inline val PlayerOffhandIndex = 40

  /** Index of the living-entity flags byte in entity metadata, for a given server version. */
  def livingFlagsIndex(server: ServerVersion): Int =
    if server.isNewerThanOrEquals(ServerVersion.V_1_17) then 8
    else if server.isNewerThanOrEquals(ServerVersion.V_1_14) then 7
    else if server.isNewerThanOrEquals(ServerVersion.V_1_10) then 6
    else 5
