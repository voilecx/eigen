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
import com.github.retrooper.packetevents.protocol.entity.data.EntityData
import com.github.retrooper.packetevents.protocol.entity.pose.EntityPose
import com.github.retrooper.packetevents.protocol.entity.`type`.{EntityType, EntityTypes}
import com.github.retrooper.packetevents.protocol.item.ItemStack
import com.github.retrooper.packetevents.protocol.packettype.PacketType
import com.github.retrooper.packetevents.protocol.player.ClientVersion
import com.github.retrooper.packetevents.protocol.teleport.RelativeFlag
import com.github.retrooper.packetevents.util.Vector3d
import com.github.retrooper.packetevents.wrapper.play.server.*

import eigen.core.entity.TrackedEntity
import eigen.core.sim.{ItemKind, PendingRotation, PendingTeleport}

/**
 * Server → client packets. Runs at the highest priority, ignoring cancelled sends so the
 * simulation only sees packets that really leave the server. Anything that changes simulation
 * state is queued on the player's [[eigen.core.latency.TransactionClock]] and therefore runs on
 * the receive path once the client has acknowledged the surrounding transaction.
 *
 * Dispatch is an O(1) array lookup indexed by `PacketType.Play.Server.ordinal` via [[PacketTable]],
 * with the server-version-specific transaction packet (`PING` on 1.17+, `WINDOW_CONFIRMATION` on
 * `< 1.17`) and living-entity metadata index resolved once at construction time through [[ServerProtocol]].
 */
final class Outbound(players: Players) extends PacketListenerAbstract(PacketListenerPriority.HIGHEST):
  import Outbound.*
  import PacketType.Play.Server as S

  private val protocol: ServerProtocol = ServerProtocol.current
  private val livingIdx: Int = protocol.livingFlagsIndex

  private val table: PacketTable[S, Handler] = PacketTable.build(S.values)(
    protocol.outboundTransactionType -> ((p, event) =>
      val id = protocol.readSentId(event)
      if id != 0 then p.clock.onPingSent(id)
    ),
    S.SPAWN_ENTITY -> ((p, event) =>
      val w = WrapperPlayServerSpawnEntity(event)
      spawn(p, w.getEntityId, w.getEntityType, w.getPosition)
    ),
    S.SPAWN_LIVING_ENTITY -> ((p, event) =>
      val w = WrapperPlayServerSpawnLivingEntity(event)
      spawn(p, w.getEntityId, w.getEntityType, w.getPosition)
    ),
    S.SPAWN_PLAYER -> ((p, event) =>
      val w = WrapperPlayServerSpawnPlayer(event)
      spawn(p, w.getEntityId, EntityTypes.PLAYER, w.getPosition)
    ),
    S.DESTROY_ENTITIES -> ((p, event) => destroy(p, WrapperPlayServerDestroyEntities(event).getEntityIds)),
    S.ENTITY_RELATIVE_MOVE -> ((p, event) =>
      val w = WrapperPlayServerEntityRelativeMove(event)
      move(p, w.getEntityId, relative = true, hasPos = true, w.getDeltaX, w.getDeltaY, w.getDeltaZ)
    ),
    S.ENTITY_RELATIVE_MOVE_AND_ROTATION -> ((p, event) =>
      val w = WrapperPlayServerEntityRelativeMoveAndRotation(event)
      move(p, w.getEntityId, relative = true, hasPos = true, w.getDeltaX, w.getDeltaY, w.getDeltaZ)
    ),
    S.ENTITY_TELEPORT -> ((p, event) =>
      val w = WrapperPlayServerEntityTeleport(event)
      absolute(p, w.getEntityId, w.getValues.getPosition)
    ),
    S.ENTITY_POSITION_SYNC -> ((p, event) =>
      val w = WrapperPlayServerEntityPositionSync(event)
      absolute(p, w.getId, w.getValues.getPosition)
    ),
    S.ENTITY_ROTATION -> ((p, event) =>
      move(p, WrapperPlayServerEntityRotation(event).getEntityId, relative = true, hasPos = false, 0, 0, 0)
    ),
    S.ENTITY_STATUS -> ((p, event) => deathStatus(p, event, WrapperPlayServerEntityStatus(event))),
    S.ENTITY_METADATA -> ((p, event) => metadata(p, WrapperPlayServerEntityMetadata(event))),
    S.UPDATE_ATTRIBUTES -> ((p, event) => attributes(p, event, WrapperPlayServerUpdateAttributes(event))),
    S.ENTITY_VELOCITY -> ((p, event) =>
      if WrapperPlayServerEntityVelocity(event).getEntityId == p.sim.selfId then knockback(p, event)
    ),
    S.EXPLOSION -> ((p, event) =>
      val motion = WrapperPlayServerExplosion(event).getKnockback
      if motion != null && (motion.getX != 0 || motion.getY != 0 || motion.getZ != 0) then knockback(p, event)
    ),
    S.PLAYER_POSITION_AND_LOOK -> ((p, event) => teleport(p, event, WrapperPlayServerPlayerPositionAndLook(event))),
    S.PLAYER_ROTATION -> ((p, event) => serverRotation(p, event, WrapperPlayServerPlayerRotation(event))),
    S.JOIN_GAME -> ((p, event) => joinGame(p, event, WrapperPlayServerJoinGame(event).getEntityId)),
    S.RESPAWN -> ((p, event) =>
      p.clock.at(p.bracket(event))(() => { p.sim.breakEpoch(); p.entityMeta.clear() })
    ),
    S.SET_PASSENGERS -> ((p, event) =>
      val w = WrapperPlayServerSetPassengers(event)
      val vehicle = w.getEntityId
      val riding = w.getPassengers.contains(p.sim.selfId)
      p.clock.at(p.bracket(event))(() => vehicleChange(p, vehicle, riding))
    ),
    S.OPEN_WINDOW -> ((p, _) => p.clock.at(p.clock.lastSent)(() => p.sim.onGui(true))),
    S.CLOSE_WINDOW -> ((p, _) => p.clock.at(p.clock.lastSent)(() => p.sim.onGui(false))),
    S.HELD_ITEM_CHANGE -> ((p, event) =>
      val slot = WrapperPlayServerHeldItemChange(event).getSlot
      p.clock.at(p.clock.lastSent)(() => p.sim.onSlot(slot))
    ),
    S.SET_SLOT -> ((p, event) =>
      val w = WrapperPlayServerSetSlot(event)
      if w.getWindowId == 0 then inventorySlot(p, w.getSlot, w.getItem)
    ),
    S.WINDOW_ITEMS -> ((p, event) =>
      val w = WrapperPlayServerWindowItems(event)
      if w.getWindowId == 0 then
        val items = w.getItems.asScala.toIndexedSeq
        for slot <- FirstHotbarSlot to OffhandSlot if slot < items.size do inventorySlot(p, slot, items(slot))
    ),
    S.SET_PLAYER_INVENTORY -> ((p, event) =>
      val w = WrapperPlayServerSetPlayerInventory(event)
      // Player inventory indices: 0..8 hotbar, 40 offhand.
      val slot = w.getSlot
      if slot >= 0 && slot < 9 then inventorySlot(p, FirstHotbarSlot + slot, w.getStack)
      else if slot == PlayerOffhandIndex then inventorySlot(p, OffhandSlot, w.getStack)
    ),
  )

  override def onPacketSend(event: PacketSendEvent): Unit =
    if event.isCancelled then return
    val tpe = event.getPacketType
    if tpe == PacketType.Login.Server.LOGIN_SUCCESS then
      val user = event.getUser
      event.getTasksAfterSend.add(() => { val _ = players.connect(user) })
    else
      val p = players.get(event.getUser)
      if p != null then
        val h = table.get(tpe)
        if h != null then h.handle(p, event)

  private def deathStatus(p: EigenPlayer, event: PacketSendEvent, w: WrapperPlayServerEntityStatus): Unit =
    if w.getStatus == DeathStatus then
      val id = w.getEntityId
      p.clock.at(p.bracket(event)): () =>
        p.sim.withEntity(id)(_.dead = true)

  private def knockback(p: EigenPlayer, event: PacketSendEvent): Unit =
    p.clock.at(p.bracket(event))(() => p.sim.onKnockback())

  private def teleport(p: EigenPlayer, event: PacketSendEvent, w: WrapperPlayServerPlayerPositionAndLook): Unit =
    val f = w.getRelativeFlags
    val pending = PendingTeleport(
      w.getX, w.getY, w.getZ, w.getYaw, w.getPitch,
      f.has(RelativeFlag.X), f.has(RelativeFlag.Y), f.has(RelativeFlag.Z), f.has(RelativeFlag.YAW), f.has(RelativeFlag.PITCH),
    )
    p.clock.at(p.bracket(event))(() => p.sim.expectTeleport(pending))

  private def serverRotation(p: EigenPlayer, event: PacketSendEvent, w: WrapperPlayServerPlayerRotation): Unit =
    val pitch = if w.isRelativePitch then w.getPitch else math.max(-90f, math.min(90f, w.getPitch % 360f))
    val rotation = PendingRotation(w.getYaw, pitch, w.isRelativeYaw, w.isRelativePitch)
    p.clock.at(p.preTransaction(event))(() => p.sim.expectRotation(rotation))

  private def joinGame(p: EigenPlayer, event: PacketSendEvent, self: Int): Unit =
    p.clock.at(p.clock.lastSent)(() => { p.sim.breakEpoch(); p.sim.selfId = self; p.entityMeta.clear() })
    p.sendAfter(event)

  private def spawn(p: EigenPlayer, id: Int, tpe: EntityType, pos: Vector3d): Unit =
    if id < 0 then return
    val t = p.spawnTransaction(id)
    val modernBoats = p.user.getClientVersion.isNewerThanOrEquals(ClientVersion.V_1_21_2)
    val e = TrackedEntity(id, EntityCatalog.kind(tpe), EntityCatalog.steps(tpe, modernBoats), p.profile)
    e.spawnAt(pos.getX, pos.getY, pos.getZ)
    val meta = EntityMeta(tpe)
    EntityCatalog.resize(e, tpe, p.profile, crouching = false, lowPose = false, scale = 1f)
    p.clock.at(t)(() => { p.entityMeta(id) = meta; p.sim.addEntity(e) })

  private def destroy(p: EigenPlayer, ids: Array[Int]): Unit =
    // Transaction bookkeeping is touched only from the sending thread.
    ids.foreach(p.noteDespawned)
    val t = p.clock.lastSent
    p.clock.at(t): () =>
      ids.foreach(id => p.sim.withEntity(id)(_.dead = true))
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
      p.sim.withEntity(id)(_.onFirstTransaction(relative, hasPos, x, y, z))
    p.clock.at(t + 1): () =>
      p.sim.withEntity(id)(_.onSecondTransaction())

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
    d match
      case MetaEntry(0, flags: java.lang.Byte) if self =>
        p.sim.onSelfFlags(gliding = (flags & 0x80) != 0, swimming = (flags & 0x10) != 0)
      case MetaEntry(i, flags: java.lang.Byte) if self && i == livingIdx =>
        p.sim.onSelfLivingFlags(riptide = (flags & 0x04) != 0)
      case MetaEntry(_, pose: EntityPose) if !self =>
        p.withTrackedEntity(id): (meta, e) =>
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
        p.withTrackedEntity(id): (meta, e) =>
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
      if slot >= FirstHotbarSlot && slot < OffhandSlot then p.sim.inventory.setHotbar(slot - FirstHotbarSlot, kind)
      else if slot == OffhandSlot then p.sim.inventory.offhand = kind

object Outbound:
  @FunctionalInterface
  trait Handler:
    def handle(p: EigenPlayer, event: PacketSendEvent): Unit

  /**
   * Zero-allocation name-based extractor for `EntityData[?]`: Scala 3 compiles pattern matches
   * against `MetaEntry(index, value)` into direct `.getIndex` and `.getValue` calls without
   * allocating an intermediate `Tuple2` or `Option`.
   */
  private object MetaEntry:
    final class Match(val raw: EntityData[?]) extends AnyVal:
      def isEmpty: Boolean = false
      def get: Match = this
      def _1: Int = raw.getIndex
      def _2: Any = raw.getValue
    inline def unapply(d: EntityData[?]): Match = Match(d)

  private inline val DeathStatus = 3
  /** Window-0 slot of the first hotbar slot; the hotbar spans nine slots. */
  private inline val FirstHotbarSlot = 36
  /** Window-0 offhand slot. */
  private inline val OffhandSlot = 45
  /** Player-inventory index of the offhand, as used by `SET_PLAYER_INVENTORY`. */
  private inline val PlayerOffhandIndex = 40
