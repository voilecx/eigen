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

package eigen.shard

import java.util.UUID
import java.util.HashMap as JHashMap

import com.github.retrooper.packetevents.PacketEvents
import com.github.retrooper.packetevents.manager.server.ServerVersion
import com.github.retrooper.packetevents.protocol.entity.pose.EntityPose
import com.github.retrooper.packetevents.protocol.entity.`type`.{EntityType, EntityTypes}
import com.github.retrooper.packetevents.protocol.player.ClientVersion
import com.github.retrooper.packetevents.protocol.world.chunk.BaseChunk
import com.github.retrooper.packetevents.protocol.world.states.WrappedBlockState
import com.github.retrooper.packetevents.util.Vector3d
import com.github.retrooper.packetevents.wrapper.play.server.WrapperPlayServerUpdateAttributes

final class ShardTrackedPosition:
  import ShardTrackedPosition.*

  var pos: Vector3d = Vector3d()

  def withDelta(x: Double, y: Double, z: Double): Vector3d = withDelta(pack(x), pack(y), pack(z))

  def withDelta(x: Long, y: Long, z: Long): Vector3d =
    if x == 0L && y == 0L && z == 0L then pos
    else
      val d = if x == 0L then pos.getX else unpack(pack(pos.getX) + x)
      val e = if y == 0L then pos.getY else unpack(pack(pos.getY) + y)
      val f = if z == 0L then pos.getZ else unpack(pack(pos.getZ) + z)
      Vector3d(d, e, f)

  def withDeltaLegacy(x: Double, y: Double, z: Double): Vector3d =
    if x == 0.0 && y == 0.0 && z == 0.0 then pos
    else
      val d = if x == 0.0 then pos.getX else unpackLegacy(packLegacy(pos.getX) + packLegacy(x))
      val e = if y == 0.0 then pos.getY else unpackLegacy(packLegacy(pos.getY) + packLegacy(y))
      val f = if z == 0.0 then pos.getZ else unpackLegacy(packLegacy(pos.getZ) + packLegacy(z))
      Vector3d(d, e, f)

  private def pack(value: Double): Long = Math.round(value * COORDINATE_SCALE)
  private def unpack(value: Long): Double = value.toDouble / COORDINATE_SCALE
  private def packLegacy(value: Double): Double = math.floor(value * COORDINATE_SCALE)
  private def unpackLegacy(value: Double): Double = value / COORDINATE_SCALE

object ShardTrackedPosition:
  inline val COORDINATE_SCALE = 4096.0

object ShardBoundingBoxSize:
  def getWidth(entity: ShardPacketEntity): Float =
    val base = getWidthBase(entity)
    val babyScaled = if entity.isBaby then base * 0.5f else base
    babyScaled * entity.scale

  def getHeight(entity: ShardPacketEntity): Float =
    val base = getHeightBase(entity)
    val babyScaled = if entity.isBaby then base * 0.5f else base
    babyScaled * entity.scale

  private val AGEABLE_HUMANOIDS = Set[EntityType](
    EntityTypes.ZOMBIE,
    EntityTypes.ZOMBIE_VILLAGER,
    EntityTypes.HUSK,
    EntityTypes.DROWNED,
    EntityTypes.SKELETON,
    EntityTypes.STRAY,
    EntityTypes.BOGGED,
    EntityTypes.PIGLIN,
    EntityTypes.PIGLIN_BRUTE,
    EntityTypes.ZOMBIFIED_PIGLIN,
    EntityTypes.VILLAGER,
    EntityTypes.WANDERING_TRADER,
    EntityTypes.VINDICATOR,
    EntityTypes.PILLAGER,
    EntityTypes.EVOKER,
    EntityTypes.ILLUSIONER,
    EntityTypes.WITCH,
    EntityTypes.BLAZE,
    EntityTypes.ENDERMAN,
    EntityTypes.BREEZE,
  )

  private def getWidthBase(entity: ShardPacketEntity): Float =
    val t = entity.tpe
    if t == EntityTypes.PLAYER || AGEABLE_HUMANOIDS.contains(t) || t == EntityTypes.CREEPER || t == EntityTypes.WOLF then 0.6f
    else if t == EntityTypes.WITHER_SKELETON || t == EntityTypes.CAVE_SPIDER || t == EntityTypes.SNOW_GOLEM || t == EntityTypes.BEE then 0.7f
    else if t == EntityTypes.SPIDER || t == EntityTypes.IRON_GOLEM then 1.4f
    else if t == EntityTypes.RAVAGER then 1.95f
    else if t == EntityTypes.GHAST || t == EntityTypes.HAPPY_GHAST then 4.0f
    else if t == EntityTypes.ENDER_DRAGON then 16.0f
    else if t == EntityTypes.WITHER || t == EntityTypes.WARDEN || t == EntityTypes.COW || t == EntityTypes.SHEEP ||
      t == EntityTypes.MOOSHROOM || t == EntityTypes.PIG || t == EntityTypes.GOAT || t == EntityTypes.LLAMA ||
      t == EntityTypes.TRADER_LLAMA || t == EntityTypes.DOLPHIN || t == EntityTypes.STRIDER
    then 0.9f
    else if t == EntityTypes.GUARDIAN then 0.85f
    else if t == EntityTypes.ELDER_GUARDIAN then 1.9975f
    else if t == EntityTypes.HORSE || t == EntityTypes.DONKEY || t == EntityTypes.MULE ||
      t == EntityTypes.SKELETON_HORSE || t == EntityTypes.ZOMBIE_HORSE || t == EntityTypes.HOGLIN || t == EntityTypes.ZOGLIN
    then 1.3964844f
    else if t == EntityTypes.PANDA then 1.3f
    else if t == EntityTypes.CHICKEN || t == EntityTypes.ENDERMITE || t == EntityTypes.SILVERFISH ||
      t == EntityTypes.VEX || t == EntityTypes.RABBIT
    then 0.4f
    else if t == EntityTypes.BAT || t == EntityTypes.PARROT || t == EntityTypes.COD ||
      t == EntityTypes.TROPICAL_FISH || t == EntityTypes.FROG || t == EntityTypes.ARMOR_STAND
    then 0.5f
    else if t == EntityTypes.AXOLOTL then 0.75f
    else if t == EntityTypes.END_CRYSTAL then 2.0f
    else if t == EntityTypes.SLIME || t == EntityTypes.MAGMA_CUBE then 0.52f * entity.slimeSize
    else if t == EntityTypes.PHANTOM then 0.9f + entity.phantomSize * 0.2f
    else if EntityTypes.isTypeInstanceOf(t, EntityTypes.BOAT) then 1.375f
    else 0.6f

  private def playerPoseHeight(pose: Int): Float =
    if pose == EntityPose.SLEEPING.ordinal then 0.2f
    else if pose == EntityPose.FALL_FLYING.ordinal || pose == EntityPose.SWIMMING.ordinal || pose == EntityPose.SPIN_ATTACK.ordinal then 0.6f
    else if pose == EntityPose.CROUCHING.ordinal then 1.5f
    else 1.8f

  private def getHeightBase(entity: ShardPacketEntity): Float =
    val t = entity.tpe
    if t == EntityTypes.PLAYER then playerPoseHeight(entity.pose)
    else if t == EntityTypes.ZOMBIE || t == EntityTypes.ZOMBIE_VILLAGER || t == EntityTypes.HUSK ||
      t == EntityTypes.DROWNED || t == EntityTypes.SKELETON || t == EntityTypes.STRAY ||
      t == EntityTypes.BOGGED || t == EntityTypes.PIGLIN || t == EntityTypes.PIGLIN_BRUTE ||
      t == EntityTypes.ZOMBIFIED_PIGLIN || t == EntityTypes.VILLAGER || t == EntityTypes.WANDERING_TRADER ||
      t == EntityTypes.VINDICATOR || t == EntityTypes.PILLAGER || t == EntityTypes.EVOKER ||
      t == EntityTypes.ILLUSIONER || t == EntityTypes.WITCH
    then 1.95f
    else if t == EntityTypes.WITHER_SKELETON then 2.4f
    else if t == EntityTypes.ENDERMAN || t == EntityTypes.WARDEN then 2.9f
    else if t == EntityTypes.SPIDER || t == EntityTypes.PIG || t == EntityTypes.BAT || t == EntityTypes.PARROT then 0.9f
    else if t == EntityTypes.CAVE_SPIDER || t == EntityTypes.RABBIT || t == EntityTypes.PHANTOM then 0.5f
    else if t == EntityTypes.CREEPER || t == EntityTypes.STRIDER then 1.7f
    else if t == EntityTypes.BLAZE then 1.8f
    else if t == EntityTypes.IRON_GOLEM then 2.7f
    else if t == EntityTypes.SNOW_GOLEM then 1.9f
    else if t == EntityTypes.RAVAGER then 2.2f
    else if t == EntityTypes.GHAST then 4.0f
    else if t == EntityTypes.ENDER_DRAGON then 8.0f
    else if t == EntityTypes.WITHER then 3.5f
    else if t == EntityTypes.GUARDIAN || t == EntityTypes.WOLF then 0.85f
    else if t == EntityTypes.ELDER_GUARDIAN then 1.9975f
    else if t == EntityTypes.BREEZE then 1.77f
    else if t == EntityTypes.COW || t == EntityTypes.MOOSHROOM || t == EntityTypes.HOGLIN || t == EntityTypes.ZOGLIN then 1.4f
    else if t == EntityTypes.SHEEP || t == EntityTypes.GOAT then 1.3f
    else if t == EntityTypes.LLAMA || t == EntityTypes.TRADER_LLAMA then 1.875f
    else if t == EntityTypes.DOLPHIN || t == EntityTypes.BEE then 0.6f
    else if t == EntityTypes.HORSE || t == EntityTypes.DONKEY || t == EntityTypes.MULE ||
      t == EntityTypes.SKELETON_HORSE || t == EntityTypes.ZOMBIE_HORSE
    then 1.6f
    else if t == EntityTypes.PANDA then 1.25f
    else if t == EntityTypes.CHICKEN then 0.7f
    else if t == EntityTypes.ENDERMITE || t == EntityTypes.SILVERFISH || t == EntityTypes.COD then 0.3f
    else if t == EntityTypes.VEX then 0.8f
    else if t == EntityTypes.TROPICAL_FISH then 0.4f
    else if t == EntityTypes.AXOLOTL then 0.42f
    else if t == EntityTypes.FROG then 0.55f
    else if t == EntityTypes.ARMOR_STAND then 1.975f
    else if t == EntityTypes.END_CRYSTAL then 2.0f
    else if t == EntityTypes.SLIME || t == EntityTypes.MAGMA_CUBE then 0.52f * entity.slimeSize
    else if t == EntityTypes.HAPPY_GHAST then 4.5f
    else if EntityTypes.isTypeInstanceOf(t, EntityTypes.BOAT) then 0.5625f
    else 1.95f

object ShardGetBoundingBox:
  def getPacketEntityBoundingBox(centerX: Double, minY: Double, centerZ: Double, entity: ShardPacketEntity): ShardSimpleCollisionBox =
    val width = ShardBoundingBoxSize.getWidth(entity)
    val height = ShardBoundingBoxSize.getHeight(entity)
    fromPosAndSize(centerX, minY, centerZ, width, height)

  def fromPosAndSize(centerX: Double, minY: Double, centerZ: Double, width: Float, height: Float): ShardSimpleCollisionBox =
    val halfWidth = width / 2.0
    val minX = centerX - halfWidth
    val maxX = centerX + halfWidth
    val maxY = minY + height
    val minZ = centerZ - halfWidth
    val maxZ = centerZ + halfWidth
    ShardSimpleCollisionBox(
      math.min(minX, maxX),
      math.min(minY, maxY),
      math.min(minZ, maxZ),
      math.max(minX, maxX),
      math.max(minY, maxY),
      math.max(minZ, maxZ),
    )

  def expandBoundingBoxByEntityDimensions(box: ShardSimpleCollisionBox, entity: ShardPacketEntity): Unit =
    val width = ShardBoundingBoxSize.getWidth(entity).toDouble
    val height = ShardBoundingBoxSize.getHeight(entity).toDouble
    val halfWidth = width / 2.0

    val minX = box.minX - halfWidth
    val minY = box.minY
    val minZ = box.minZ - halfWidth
    val maxX = box.maxX + halfWidth
    val maxY = box.maxY + height
    val maxZ = box.maxZ + halfWidth

    box.minX = math.min(minX, maxX)
    box.minY = math.min(minY, maxY)
    box.minZ = math.min(minZ, maxZ)
    box.maxX = math.max(minX, maxX)
    box.maxY = math.max(minY, maxY)
    box.maxZ = math.max(minZ, maxZ)

final class ShardReachInterpolationData(
    var startingLocation: ShardSimpleCollisionBox,
    private val targetLocation: ShardSimpleCollisionBox,
    private val entity: ShardPacketEntity,
):
  import ShardReachInterpolationData.*

  private var interpolationStepsLowBound: Int = 0
  private var interpolationStepsHighBound: Int = 0
  private var interpolationSteps: Int = 1
  private var shouldExpandNonRelative: Boolean = false

  private var maxOffsetX: Double = 0.0
  private var maxOffsetY: Double = 0.0
  private var maxOffsetZ: Double = 0.0
  private var hasMaxOffset: Boolean = false
  private var teleportActive: Boolean = false

  def this(
      player: ShardPlayer,
      startingLocation: ShardSimpleCollisionBox,
      position: ShardTrackedPosition,
      entity: ShardPacketEntity,
  ) =
    this(
      startingLocation,
      {
        val pos = position.pos
        ShardSimpleCollisionBox(pos.getX, pos.getY, pos.getZ, pos.getX, pos.getY, pos.getZ)
      },
      entity,
    )
    val canSkipTicks = player.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_9)
    val unreliableTicking = player.compensatedEntities.self.riding == null && canSkipTicks

    if player.clientVersion.isOlderThan(ClientVersion.V_1_9) &&
      ShardReachInterpolationData.isServerV1_9OrNewer
    then targetLocation.expand(0.03125)

    interpolationSteps = interpolationStepsFor(entity, player.clientVersion)
    if unreliableTicking then interpolationStepsHighBound = interpolationSteps

    buildMaxOffset(player.clientVersion)
    clampStartToTarget()

  def this(startingLocation: ShardSimpleCollisionBox, entity: ShardPacketEntity) =
    this(startingLocation, startingLocation, entity)

  def getPossibleLocationCombined(): ShardSimpleCollisionBox =
    val steps = interpolationSteps.toDouble
    val stepMinX = (targetLocation.minX - startingLocation.minX) / steps
    val stepMaxX = (targetLocation.maxX - startingLocation.maxX) / steps
    val stepMinY = (targetLocation.minY - startingLocation.minY) / steps
    val stepMaxY = (targetLocation.maxY - startingLocation.maxY) / steps
    val stepMinZ = (targetLocation.minZ - startingLocation.minZ) / steps
    val stepMaxZ = (targetLocation.maxZ - startingLocation.maxZ) / steps

    var result = boxAtStep(interpolationStepsLowBound, stepMinX, stepMaxX, stepMinY, stepMaxY, stepMinZ, stepMaxZ)
    var step = interpolationStepsLowBound + 1
    while step <= interpolationStepsHighBound do
      result = ShardSimpleCollisionBox.combine(
        result,
        boxAtStep(step, stepMinX, stepMaxX, stepMinY, stepMaxY, stepMinZ, stepMaxZ),
      )
      step += 1
    result

  def getPossibleHitboxCombined(): ShardSimpleCollisionBox =
    val combined = getPossibleLocationCombined()
    if shouldExpandNonRelative then combined.expand(0.03125, 0.015625, 0.03125)
    ShardGetBoundingBox.expandBoundingBoxByEntityDimensions(combined, entity)
    combined

  def tickMovement(incrementLowBound: Boolean, tickingReliably: Boolean): Unit =
    if !tickingReliably then interpolationStepsHighBound = interpolationSteps
    if incrementLowBound then interpolationStepsLowBound = math.min(interpolationStepsLowBound + 1, interpolationSteps)
    interpolationStepsHighBound = math.min(interpolationStepsHighBound + 1, interpolationSteps)

  def updatePossibleStartingLocation(possibleLocationCombined: ShardSimpleCollisionBox): Unit =
    startingLocation = ShardSimpleCollisionBox.combine(startingLocation, possibleLocationCombined)
    clampStartToTarget()

  def expandNonRelative(): Unit =
    shouldExpandNonRelative = true

  private def buildMaxOffset(clientVersion: ClientVersion): Unit =
    val steps = interpolationSteps
    val legacy = clientVersion.isOlderThan(ClientVersion.V_1_9) && ShardReachInterpolationData.isServerV1_9OrNewer
    val jitterXZ = if legacy then LEGACY_JITTER_XZ else JITTER
    val jitterY = if legacy then LEGACY_JITTER_Y else JITTER

    var offsetX = steps * math.max(entity.interpVelEstimate(0), VELOCITY_FLOOR_XZ) * SAFETY + jitterXZ
    var offsetY = steps * math.max(entity.interpVelEstimate(1), VELOCITY_FLOOR_Y) * SAFETY + jitterY
    var offsetZ = steps * math.max(entity.interpVelEstimate(2), VELOCITY_FLOOR_XZ) * SAFETY + jitterXZ

    val jump = entity.lastTeleportJump
    if jump != null then
      offsetX = math.max(offsetX, jump(0) + jitterXZ)
      offsetY = math.max(offsetY, jump(1) + jitterY)
      offsetZ = math.max(offsetZ, jump(2) + jitterXZ)
      teleportActive = true
    entity.lastTeleportJump = null

    maxOffsetX = offsetX
    maxOffsetY = offsetY
    maxOffsetZ = offsetZ
    hasMaxOffset = true

  private def clampStartToTarget(): Unit =
    if !hasMaxOffset then return

    val box = startingLocation
    val centerX = (targetLocation.minX + targetLocation.maxX) * HALF
    val centerY = (targetLocation.minY + targetLocation.maxY) * HALF
    val centerZ = (targetLocation.minZ + targetLocation.maxZ) * HALF

    var minX = math.max(box.minX, centerX - maxOffsetX)
    var maxX = math.min(box.maxX, centerX + maxOffsetX)
    var minY = math.max(box.minY, centerY - maxOffsetY)
    var maxY = math.min(box.maxY, centerY + maxOffsetY)
    var minZ = math.max(box.minZ, centerZ - maxOffsetZ)
    var maxZ = math.min(box.maxZ, centerZ + maxOffsetZ)

    if minX > maxX then
      minX = if teleportActive then box.minX else centerX
      maxX = if teleportActive then box.maxX else centerX
    if minY > maxY then
      minY = if teleportActive then box.minY else centerY
      maxY = if teleportActive then box.maxY else centerY
    if minZ > maxZ then
      minZ = if teleportActive then box.minZ else centerZ
      maxZ = if teleportActive then box.maxZ else centerZ

    box.minX = minX
    box.minY = minY
    box.minZ = minZ
    box.maxX = maxX
    box.maxY = maxY
    box.maxZ = maxZ

  private def boxAtStep(
      step: Int,
      stepMinX: Double,
      stepMaxX: Double,
      stepMinY: Double,
      stepMaxY: Double,
      stepMinZ: Double,
      stepMaxZ: Double,
  ): ShardSimpleCollisionBox =
    ShardSimpleCollisionBox(
      startingLocation.minX + step * stepMinX,
      startingLocation.minY + step * stepMinY,
      startingLocation.minZ + step * stepMinZ,
      startingLocation.maxX + step * stepMaxX,
      startingLocation.maxY + step * stepMaxY,
      startingLocation.maxZ + step * stepMaxZ,
    )

object ShardReachInterpolationData:
  lazy val isServerV1_9OrNewer: Boolean =
    try PacketEvents.getAPI.getServerManager.getVersion.isNewerThanOrEquals(ServerVersion.V_1_9)
    catch case _: Throwable => true

  def interpolationStepsFor(entity: ShardPacketEntity, clientVersion: ClientVersion): Int =
    if entity.isBoat then
      if clientVersion.isNewerThanOrEquals(ClientVersion.V_1_21_2) then GENERIC_STEPS else BOAT_STEPS
    else if entity.isMinecart then MINECART_STEPS
    else if entity.tpe == EntityTypes.SHULKER then 1
    else if entity.isLivingEntity then GENERIC_STEPS
    else 1

  inline val BOAT_STEPS = 10
  inline val MINECART_STEPS = 5
  inline val GENERIC_STEPS = 3
  inline val HALF = 0.5
  inline val VELOCITY_FLOOR_XZ = 0.05
  inline val VELOCITY_FLOOR_Y = 0.08
  inline val JITTER = 1e-3
  inline val LEGACY_JITTER_XZ = 0.03125
  inline val LEGACY_JITTER_Y = 0.015625
  inline val SAFETY = 1.1

open class ShardPacketEntity(val player: ShardPlayer, val uuid: UUID, val tpe: EntityType):
  import ShardPacketEntity.*

  val isPlayer: Boolean = tpe == EntityTypes.PLAYER
  val isLivingEntity: Boolean = EntityTypes.isTypeInstanceOf(tpe, EntityTypes.LIVINGENTITY)
  val isBoat: Boolean = EntityTypes.isTypeInstanceOf(tpe, EntityTypes.BOAT)
  val isMinecart: Boolean = EntityTypes.isTypeInstanceOf(tpe, EntityTypes.MINECART_ABSTRACT)

  @volatile var riding: ShardPacketEntity | Null = null

  val trackedServerPosition: ShardTrackedPosition = ShardTrackedPosition()

  var newPacketLocation: ShardReachInterpolationData | Null = null
  var oldPacketLocation: ShardReachInterpolationData | Null = null

  var isBaby: Boolean = false
  var isDead: Boolean = false
  var slimeSize: Int = 1
  var phantomSize: Int = 0
  var scale: Float = 1.0f
  var pose: Int = 0

  var lastTransactionHung: Int = 0

  val interpVelEstimate: Array[Double] = new Array[Double](AXIS_COUNT)
  private var lastTrackedTargetPos: Vector3d | Null = null
  var lastTeleportJump: Array[Double] | Null = null

  def canHit(): Boolean = !isDead

  def mount(vehicle: ShardPacketEntity): Unit =
    riding = vehicle

  def eject(): Unit =
    riding = null

  def onFirstTransaction(
      relative: Boolean,
      hasPos: Boolean,
      deltaX: Double,
      deltaY: Double,
      deltaZ: Double,
      player: ShardPlayer,
  ): Unit =
    val preMovePos = trackedServerPosition.pos
    val old = newPacketLocation
    val startBox =
      if old != null then old.getPossibleLocationCombined()
      else
        val pos = trackedServerPosition.pos
        ShardSimpleCollisionBox(pos.getX, pos.getY, pos.getZ, pos.getX, pos.getY, pos.getZ)

    if hasPos then
      trackedServerPosition.pos =
        if relative then
          if player.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_19_3) then
            trackedServerPosition.withDelta(deltaX, deltaY, deltaZ)
          else trackedServerPosition.withDeltaLegacy(deltaX, deltaY, deltaZ)
        else if player.clientVersion.isOlderThan(ClientVersion.V_1_9) then
          Vector3d(
            (deltaX * LEGACY_TELEPORT_SCALE).toInt / LEGACY_TELEPORT_SCALE,
            (deltaY * LEGACY_TELEPORT_SCALE).toInt / LEGACY_TELEPORT_SCALE,
            (deltaZ * LEGACY_TELEPORT_SCALE).toInt / LEGACY_TELEPORT_SCALE,
          )
        else Vector3d(deltaX, deltaY, deltaZ)

    if hasPos then updateInterpolationBounds(relative, preMovePos, player.clientVersion)

    oldPacketLocation = old
    newPacketLocation =
      if !hasPos && rotationFreezesInterpolation(player.clientVersion) then
        ShardReachInterpolationData(startBox, this)
      else ShardReachInterpolationData(player, startBox, trackedServerPosition, this)

    if hasPos && !relative then applyNonRelativeStall(deltaX, deltaY, deltaZ)

  private def updateInterpolationBounds(relative: Boolean, preMovePos: Vector3d, clientVersion: ClientVersion): Unit =
    val now = trackedServerPosition.pos
    val last = lastTrackedTargetPos
    if last != null then
      val steps = ShardReachInterpolationData.interpolationStepsFor(this, clientVersion)
      val decay = if steps <= 1 then 0.0 else (steps - 1.0) / steps
      val deltas = Array(math.abs(now.getX - last.getX), math.abs(now.getY - last.getY), math.abs(now.getZ - last.getZ))
      var axis = 0
      while axis < deltas.length do
        val decayed = interpVelEstimate(axis) * decay
        val delta = deltas(axis)
        interpVelEstimate(axis) = if delta < SNAP_VELOCITY_SAMPLE_CAP then math.max(decayed, delta) else decayed
        axis += 1
    lastTrackedTargetPos = now

    val jumpX = math.abs(now.getX - preMovePos.getX)
    val jumpY = math.abs(now.getY - preMovePos.getY)
    val jumpZ = math.abs(now.getZ - preMovePos.getZ)
    val lerped = jumpX < TELEPORT_SNAP_DISTANCE && jumpY < TELEPORT_SNAP_DISTANCE && jumpZ < TELEPORT_SNAP_DISTANCE
    lastTeleportJump = if relative || !lerped then null else Array(jumpX, jumpY, jumpZ)

  private def applyNonRelativeStall(x: Double, y: Double, z: Double): Unit =
    if player.clientVersion.isNewerThan(ClientVersion.V_1_16_1) then return
    val newLoc = newPacketLocation
    if newLoc == null then return
    val area = newLoc.getPossibleLocationCombined()
    val stalled =
      area.distanceX(x) < NON_RELATIVE_THRESHOLD_XZ &&
        area.distanceY(y) < NON_RELATIVE_THRESHOLD_Y &&
        area.distanceZ(z) < NON_RELATIVE_THRESHOLD_XZ
    if stalled then newLoc.expandNonRelative()

  private def rotationFreezesInterpolation(clientVersion: ClientVersion): Boolean =
    (clientVersion.isOlderThan(ClientVersion.V_1_21_9) && clientVersion.isNewerThan(ClientVersion.V_1_21_4)) ||
      (clientVersion.isOlderThan(ClientVersion.V_1_20_2) && clientVersion.isNewerThan(ClientVersion.V_1_14_4))

  def onSecondTransaction(): Unit =
    oldPacketLocation = null

  def onMovement(tickingReliably: Boolean): Unit =
    val newLoc = newPacketLocation
    if newLoc == null then return
    newLoc.tickMovement(incrementLowBound = oldPacketLocation == null, tickingReliably = tickingReliably)
    val old = oldPacketLocation
    if old != null then
      old.tickMovement(incrementLowBound = true, tickingReliably = tickingReliably)
      newLoc.updatePossibleStartingLocation(old.getPossibleLocationCombined())

  def getPossibleCollisionBoxes(): ShardSimpleCollisionBox =
    val nl = newPacketLocation
    val ol = oldPacketLocation
    val newBox = if nl != null then nl.getPossibleHitboxCombined() else null
    val oldBox = if ol != null then ol.getPossibleHitboxCombined() else null
    if newBox != null && oldBox != null then ShardSimpleCollisionBox.combine(newBox, oldBox)
    else if newBox != null then newBox
    else if oldBox != null then oldBox
    else
      val pos = trackedServerPosition.pos
      ShardGetBoundingBox.getPacketEntityBoundingBox(pos.getX, pos.getY, pos.getZ, this)

object ShardPacketEntity:
  inline val AXIS_COUNT = 3
  inline val LEGACY_TELEPORT_SCALE = 32.0
  inline val NON_RELATIVE_THRESHOLD_XZ = 0.03125
  inline val NON_RELATIVE_THRESHOLD_Y = 0.015625
  inline val SNAP_VELOCITY_SAMPLE_CAP = 8.0
  inline val TELEPORT_SNAP_DISTANCE = 64.0

final class ShardPacketEntitySelf(player: ShardPlayer) extends ShardPacketEntity(player, player.uuid, EntityTypes.PLAYER)

final class ShardCompensatedEntities(private val player: ShardPlayer):
  import ShardCompensatedEntities.*

  val entityMap: JHashMap[java.lang.Integer, ShardPacketEntity] = JHashMap()
  val self: ShardPacketEntitySelf = ShardPacketEntitySelf(player)

  def addEntity(
      entityId: Int,
      uuid: UUID,
      tpe: EntityType,
      x: Double = 0.0,
      y: Double = 0.0,
      z: Double = 0.0,
  ): Unit =
    val packetEntity = ShardPacketEntity(player, uuid, tpe)
    packetEntity.trackedServerPosition.pos =
      if player.clientVersion.isOlderThan(ClientVersion.V_1_9) then
        Vector3d(
          (x * LEGACY_SPAWN_SCALE).toInt / LEGACY_SPAWN_SCALE,
          (y * LEGACY_SPAWN_SCALE).toInt / LEGACY_SPAWN_SCALE,
          (z * LEGACY_SPAWN_SCALE).toInt / LEGACY_SPAWN_SCALE,
        )
      else Vector3d(x, y, z)
    entityMap.put(entityId, packetEntity)

  def getEntity(entityId: Int): ShardPacketEntity | Null =
    if entityId == player.entityId then self
    else entityMap.get(entityId)

  def removeEntity(entityId: Int): Unit =
    entityMap.remove(entityId)

  def clear(): Unit =
    self.eject()
    entityMap.clear()

  def updateAttributes(entityId: Int, properties: java.util.List[WrapperPlayServerUpdateAttributes.Property]): Unit =
    val entity = getEntity(entityId)
    if entity == null then return
    val isSelf = entityId == player.entityId
    val tracking = player.tracking
    val iter = properties.iterator()
    while iter.hasNext do
      val prop = iter.next()
      val attr = prop.getAttribute
      val key = if attr != null then attr.getName.toString else null
      if key != null then
        val lower = key.toLowerCase(java.util.Locale.ROOT)
        if lower.endsWith(":scale") || lower == "minecraft:generic.scale" then
          val s = prop.calcValue().toFloat
          entity.scale = s
          if isSelf then tracking.scale = s
        else if isSelf then
          if lower.contains("movement_speed") then
            prop.removeModifierIf(mod =>
              mod.getUUID == SPRINTING_MODIFIER_UUID ||
                mod.getName.getKey.equalsIgnoreCase("sprinting")
            )
            tracking.movementSpeed = prop.calcValue().toFloat
          else if lower.contains("entity_interaction_range") then
            tracking.entityInteractionRange = prop.calcValue().toFloat
          else if lower.contains("gravity") then
            tracking.gravity = prop.calcValue().toFloat
          else if lower.contains("jump_strength") then
            tracking.jumpStrength = prop.calcValue().toFloat
          else if lower.contains("step_height") then
            tracking.stepHeight = prop.calcValue().toFloat
          else if lower.contains("water_movement_efficiency") then
            tracking.waterMovementEfficiency = prop.calcValue().toFloat

object ShardCompensatedEntities:
  inline val LEGACY_SPAWN_SCALE = 32.0
  val SPRINTING_MODIFIER_UUID: UUID = UUID.fromString("662a6b8d-da3e-4c1c-8813-96ea6097278d")

final class ShardCompensatedFireworks:
  private val activeFireworks = java.util.HashSet[Int]()
  private val fireworksToRemoveNextTick = java.util.HashSet[Int]()

  def onTickEnd(): Unit =
    if !fireworksToRemoveNextTick.isEmpty then
      activeFireworks.removeAll(fireworksToRemoveNextTick)
      fireworksToRemoveNextTick.clear()

  def hasFirework(entityId: Int): Boolean = activeFireworks.contains(entityId)
  def addNewFirework(entityId: Int): Unit = { val _ = activeFireworks.add(entityId) }
  def removeFirework(entityId: Int): Unit =
    if activeFireworks.contains(entityId) then { val _ = fireworksToRemoveNextTick.add(entityId) }
  def count(): Int = activeFireworks.size()

final class ShardCompensatedWorld(private val player: ShardPlayer):
  import ShardCompensatedWorld.*

  private val chunks = JHashMap[java.lang.Long, Array[BaseChunk | Null]]()
  private var minSectionYOverride: Int | Null = null

  def onChunkLoad(chunkX: Int, chunkZ: Int, sections: Array[BaseChunk]): Unit =
    val copy = new Array[BaseChunk | Null](sections.length)
    System.arraycopy(sections, 0, copy, 0, sections.length)
    chunks.put(chunkKey(chunkX, chunkZ), copy)

  def onChunkUnload(chunkX: Int, chunkZ: Int): Unit =
    chunks.remove(chunkKey(chunkX, chunkZ))

  def updateMinHeight(minY: Int): Unit =
    minSectionYOverride = minY >> 4

  def setBlock(x: Int, y: Int, z: Int, state: WrappedBlockState | Null): Unit =
    val sections = chunks.get(chunkKey(x >> 4, z >> 4))
    if sections == null then return
    val sectionIndex = (y >> 4) - minSectionY()
    if sectionIndex < 0 || sectionIndex >= sections.length then return
    val section = sections(sectionIndex)
    if section == null then return
    if state != null then section.set(x & 0xf, y & 0xf, z & 0xf, state.getGlobalId)

  def getBlock(x: Int, y: Int, z: Int): WrappedBlockState | Null =
    val sections = chunks.get(chunkKey(x >> 4, z >> 4))
    if sections == null then return null
    val sectionIndex = (y >> 4) - minSectionY()
    if sectionIndex < 0 || sectionIndex >= sections.length then return null
    val section = sections(sectionIndex)
    if section == null then return null
    section.get(x & 0xf, y & 0xf, z & 0xf)

  def clear(): Unit =
    chunks.clear()

  private def minSectionY(): Int =
    val over = minSectionYOverride
    if over != null then over.asInstanceOf[Int]
    else if player.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_18) then -4 else 0

object ShardCompensatedWorld:
  private def chunkKey(chunkX: Int, chunkZ: Int): Long =
    (chunkX.toLong & 0xffffffffL) | ((chunkZ.toLong & 0xffffffffL) << 32)
