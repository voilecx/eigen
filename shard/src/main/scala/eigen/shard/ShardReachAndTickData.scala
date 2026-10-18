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

import java.util.ArrayList

import com.github.retrooper.packetevents.PacketEvents
import com.github.retrooper.packetevents.manager.server.ServerVersion
import com.github.retrooper.packetevents.protocol.entity.`type`.EntityTypes
import com.github.retrooper.packetevents.protocol.item.`type`.{ItemType, ItemTypes}
import com.github.retrooper.packetevents.protocol.player.ClientVersion

object ShardReachUtils:
  private inline val DEGREES_TO_RADIANS = 0.017453292f
  private inline val EPSILON = 1.0000000116860974e-7
  private inline val OCCLUSION_STEP = 0.25
  private inline val OCCLUSION_MAX_SAMPLES = 24
  private inline val HALF = 0.5
  private inline val EXPOSURE_SAMPLE_POINTS = 9f
  private inline val BOX_CORNERS = 8
  private inline val CORNER_BIT_X = 1
  private inline val CORNER_BIT_Y = 2
  private inline val CORNER_BIT_Z = 4
  private inline val STAND_EYE_HEIGHT = 1.62
  private inline val SNEAK_EYE_HEIGHT = 1.27
  private inline val SWIM_EYE_HEIGHT = 0.4
  private inline val MIN_AIM_DISTANCE = 0.001
  private inline val MIN_ANGULAR_EXTENT = 1e-6
  private inline val FULL_TURN_DEGREES = 360.0

  private val POSSIBLE_EYE_HEIGHTS: Array[Double] = Array(1.62, 1.27, 0.4)

  def getPossibleEyeHeights(): Array[Double] = POSSIBLE_EYE_HEIGHTS

  def calculateIntercept(box: ShardSimpleCollisionBox, origin: ShardVector3dm, end: ShardVector3dm): ShardVector3dm | Null =
    var minX = getIntermediateWithXValue(origin, end, box.minX)
    var maxX = getIntermediateWithXValue(origin, end, box.maxX)
    var minY = getIntermediateWithYValue(origin, end, box.minY)
    var maxY = getIntermediateWithYValue(origin, end, box.maxY)
    var minZ = getIntermediateWithZValue(origin, end, box.minZ)
    var maxZ = getIntermediateWithZValue(origin, end, box.maxZ)

    if !isVecInYZ(box, minX) then minX = null
    if !isVecInYZ(box, maxX) then maxX = null
    if !isVecInXZ(box, minY) then minY = null
    if !isVecInXZ(box, maxY) then maxY = null
    if !isVecInXY(box, minZ) then minZ = null
    if !isVecInXY(box, maxZ) then maxZ = null

    var best: ShardVector3dm | Null = null
    if minX != null then best = minX
    if maxX != null && (best == null || origin.distanceSquared(maxX) < origin.distanceSquared(best)) then best = maxX
    if minY != null && (best == null || origin.distanceSquared(minY) < origin.distanceSquared(best)) then best = minY
    if maxY != null && (best == null || origin.distanceSquared(maxY) < origin.distanceSquared(best)) then best = maxY
    if minZ != null && (best == null || origin.distanceSquared(minZ) < origin.distanceSquared(best)) then best = minZ
    if maxZ != null && (best == null || origin.distanceSquared(maxZ) < origin.distanceSquared(best)) then best = maxZ
    best

  def isVecInside(box: ShardSimpleCollisionBox, vec: ShardVector3dm): Boolean =
    vec.x > box.minX &&
      vec.x < box.maxX &&
      vec.y > box.minY &&
      vec.y < box.maxY &&
      vec.z > box.minZ &&
      vec.z < box.maxZ

  def getMinReachToBox(player: ShardPlayer, targetBox: ShardSimpleCollisionBox): Double =
    var lowest = Double.MaxValue
    val eyeHeights = getPossibleEyeHeights()
    var i = 0
    while i < eyeHeights.length do
      val eyeHeight = eyeHeights(i)
      val closest = cutBoxToVector(player.movement.x, player.movement.y + eyeHeight, player.movement.z, targetBox)
      lowest = math.min(lowest, closest.distance(player.movement.x, player.movement.y + eyeHeight, player.movement.z))
      i += 1
    lowest

  def minDistanceRayToBox(origin: ShardVector3dm, dir: ShardVector3dm, maxDist: Double, box: ShardSimpleCollisionBox): Double =
    if calculateIntercept(
        box,
        origin,
        ShardVector3dm(origin.x + dir.x * maxDist, origin.y + dir.y * maxDist, origin.z + dir.z * maxDist),
      ) != null
    then return 0.0
    if isVecInside(box, origin) then return 0.0

    val samples = 32
    var minD2 = Double.MaxValue
    var i = 0
    while i <= samples do
      val t = maxDist * i / samples
      val px = origin.x + dir.x * t
      val py = origin.y + dir.y * t
      val pz = origin.z + dir.z * t
      val cx = math.max(box.minX, math.min(box.maxX, px))
      val cy = math.max(box.minY, math.min(box.maxY, py))
      val cz = math.max(box.minZ, math.min(box.maxZ, pz))
      val dx = px - cx
      val dy = py - cy
      val dz = pz - cz
      val d2 = dx * dx + dy * dy + dz * dz
      if d2 < minD2 then minD2 = d2
      i += 1
    math.sqrt(minD2)

  def getMinAngleToBox(player: ShardPlayer, box: ShardSimpleCollisionBox): Double =
    val lookDirs = getPossibleLookDirs(player)
    val eyeHeights = getPossibleEyeHeights()
    val points: Array[Array[Double]] = Array(
      Array((box.minX + box.maxX) / 2, (box.minY + box.maxY) / 2, (box.minZ + box.maxZ) / 2),
      Array(box.minX, box.minY, box.minZ),
      Array(box.maxX, box.minY, box.minZ),
      Array(box.minX, box.maxY, box.minZ),
      Array(box.maxX, box.maxY, box.minZ),
      Array(box.minX, box.minY, box.maxZ),
      Array(box.maxX, box.minY, box.maxZ),
      Array(box.minX, box.maxY, box.maxZ),
      Array(box.maxX, box.maxY, box.maxZ),
    )

    var minAngle = Double.MaxValue
    var dIdx = 0
    while dIdx < lookDirs.length do
      val lookVec = lookDirs(dIdx)
      var eIdx = 0
      while eIdx < eyeHeights.length do
        val eye = eyeHeights(eIdx)
        val ex = player.movement.x
        val ey = player.movement.y + eye
        val ez = player.movement.z
        var pIdx = 0
        while pIdx < points.length do
          val p = points(pIdx)
          val dx = p(0) - ex
          val dy = p(1) - ey
          val dz = p(2) - ez
          val len = math.sqrt(dx * dx + dy * dy + dz * dz)
          if len < 0.001 then return 0.0
          val dot = lookVec.x * (dx / len) + lookVec.y * (dy / len) + lookVec.z * (dz / len)
          val clamped = math.max(-1.0, math.min(1.0, dot))
          minAngle = math.min(minAngle, Math.toDegrees(math.acos(clamped)))
          pIdx += 1
        eIdx += 1
      dIdx += 1
    minAngle

  def aimError(player: ShardPlayer, box: ShardSimpleCollisionBox): Double =
    val cx = (box.minX + box.maxX) / 2
    val cy = (box.minY + box.maxY) / 2
    val cz = (box.minZ + box.maxZ) / 2

    val lookDirs = getPossibleLookDirs(player)
    val eyeHeights = getPossibleEyeHeights()
    var minAngle = Double.MaxValue
    var distAtMin = 0.0
    var dIdx = 0
    while dIdx < lookDirs.length do
      val lookVec = lookDirs(dIdx)
      var eIdx = 0
      while eIdx < eyeHeights.length do
        val eye = eyeHeights(eIdx)
        val dx = cx - player.movement.x
        val dy = cy - (player.movement.y + eye)
        val dz = cz - player.movement.z
        val len = math.sqrt(dx * dx + dy * dy + dz * dz)
        if len < 0.001 then return 0.0
        val dot = lookVec.x * (dx / len) + lookVec.y * (dy / len) + lookVec.z * (dz / len)
        val angle = Math.toDegrees(math.acos(math.max(-1.0, math.min(1.0, dot))))
        if angle < minAngle then
          minAngle = angle
          distAtMin = len
        eIdx += 1
      dIdx += 1

    if minAngle == Double.MaxValue then return 0.0
    val halfExtent = math.max(box.maxX - box.minX, math.max(box.maxY - box.minY, box.maxZ - box.minZ)) / 2
    val angularRadius = Math.toDegrees(math.atan2(halfExtent, distAtMin))
    if angularRadius > 1e-6 then minAngle / angularRadius else 0.0

  private def radians(degrees: Float): Float = degrees * DEGREES_TO_RADIANS

  def getLook(player: ShardPlayer, yaw: Float, pitch: Float): ShardVector3dm =
    val modern = player.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_21_11)
    val look =
      if player.clientVersion.isOlderThanOrEquals(ClientVersion.V_1_12_2) then
        val yawRad = radians(-yaw) - Math.PI.toFloat
        val pitchRad = radians(-pitch)
        val pitchCos = -ShardVanillaMath.cos(pitchRad, modern)
        ShardVector3dm(
          (ShardVanillaMath.sin(yawRad, modern) * pitchCos).toDouble,
          ShardVanillaMath.sin(pitchRad, modern).toDouble,
          (ShardVanillaMath.cos(yawRad, modern) * pitchCos).toDouble,
        )
      else
        val pitchRad = radians(pitch)
        val yawRad = radians(-yaw)
        val pitchCos = ShardVanillaMath.cos(pitchRad, modern)
        ShardVector3dm(
          (ShardVanillaMath.sin(yawRad, modern) * pitchCos).toDouble,
          (-ShardVanillaMath.sin(pitchRad, modern)).toDouble,
          (ShardVanillaMath.cos(yawRad, modern) * pitchCos).toDouble,
        )
    val length = math.sqrt(look.x * look.x + look.y * look.y + look.z * look.z)
    if length == 0.0 then look
    else ShardVector3dm(look.x / length, look.y / length, look.z / length)

  def getPossibleLookDirs(player: ShardPlayer): Array[ShardVector3dm] =
    val m = player.movement
    val version = player.clientVersion
    if version.isOlderThan(ClientVersion.V_1_8) then
      Array(getLook(player, m.yaw, m.pitch))
    else if version.isNewerThanOrEquals(ClientVersion.V_1_9) then
      Array(
        getLook(player, m.yaw, m.pitch),
        getLook(player, m.lastYaw, m.pitch),
        getLook(player, m.lastYaw, m.lastPitch),
      )
    else
      Array(
        getLook(player, m.yaw, m.pitch),
        getLook(player, m.lastYaw, m.pitch),
      )

  def cutBoxToVector(x: Double, y: Double, z: Double, box: ShardSimpleCollisionBox): ShardVector3dm =
    ShardVector3dm(
      math.max(box.minX, math.min(box.maxX, x)),
      math.max(box.minY, math.min(box.maxY, y)),
      math.max(box.minZ, math.min(box.maxZ, z)),
    )

  private def getIntermediateWithXValue(self: ShardVector3dm, other: ShardVector3dm, x: Double): ShardVector3dm | Null =
    val dx = other.x - self.x
    val dy = other.y - self.y
    val dz = other.z - self.z
    if dx * dx < EPSILON then null
    else
      val t = (x - self.x) / dx
      if t >= 0.0 && t <= 1.0 then ShardVector3dm(self.x + dx * t, self.y + dy * t, self.z + dz * t) else null

  private def getIntermediateWithYValue(self: ShardVector3dm, other: ShardVector3dm, y: Double): ShardVector3dm | Null =
    val dx = other.x - self.x
    val dy = other.y - self.y
    val dz = other.z - self.z
    if dy * dy < EPSILON then null
    else
      val t = (y - self.y) / dy
      if t >= 0.0 && t <= 1.0 then ShardVector3dm(self.x + dx * t, self.y + dy * t, self.z + dz * t) else null

  private def getIntermediateWithZValue(self: ShardVector3dm, other: ShardVector3dm, z: Double): ShardVector3dm | Null =
    val dx = other.x - self.x
    val dy = other.y - self.y
    val dz = other.z - self.z
    if dz * dz < EPSILON then null
    else
      val t = (z - self.z) / dz
      if t >= 0.0 && t <= 1.0 then ShardVector3dm(self.x + dx * t, self.y + dy * t, self.z + dz * t) else null

  def exposureFraction(player: ShardPlayer, tightBox: ShardSimpleCollisionBox): Float =
    val eye = eyeOrigin(player)
    val cx = (tightBox.minX + tightBox.maxX) * HALF
    val cy = (tightBox.minY + tightBox.maxY) * HALF
    val cz = (tightBox.minZ + tightBox.maxZ) * HALF
    var visible = 0
    if !isRayOccluded(player, eye, cx, cy, cz) then visible += 1
    var corner = 0
    while corner < BOX_CORNERS do
      val px = if (corner & CORNER_BIT_X) == 0 then tightBox.minX else tightBox.maxX
      val py = if (corner & CORNER_BIT_Y) == 0 then tightBox.minY else tightBox.maxY
      val pz = if (corner & CORNER_BIT_Z) == 0 then tightBox.minZ else tightBox.maxZ
      if !isRayOccluded(player, eye, px, py, pz) then visible += 1
      corner += 1
    visible / EXPOSURE_SAMPLE_POINTS

  def eyeOrigin(player: ShardPlayer): ShardVector3dm =
    val m = player.movement
    val eyeHeight = getPossibleEyeHeights()(0)
    ShardVector3dm(m.x, m.y + eyeHeight, m.z)

  def isRayOccluded(player: ShardPlayer, origin: ShardVector3dm, tx: Double, ty: Double, tz: Double): Boolean =
    val dx = tx - origin.x
    val dy = ty - origin.y
    val dz = tz - origin.z
    val dist = math.sqrt(dx * dx + dy * dy + dz * dz)
    if dist < OCCLUSION_STEP then return false

    val samples = math.min((dist / OCCLUSION_STEP).toInt, OCCLUSION_MAX_SAMPLES)
    val scale = OCCLUSION_STEP / dist
    val sx = dx * scale
    val sy = dy * scale
    val sz = dz * scale

    var step = 1
    while step < samples do
      val block = player.compensatedWorld.getBlock(
        math.floor(origin.x + sx * step).toInt,
        math.floor(origin.y + sy * step).toInt,
        math.floor(origin.z + sz * step).toInt,
      )
      if block != null && block.getType.isSolid && !block.getType.isAir then return true
      step += 1
    false

  def boxInflationExtent(union: ShardSimpleCollisionBox, tight: ShardSimpleCollisionBox): Double =
    ((union.maxX - union.minX) - (tight.maxX - tight.minX)) +
      ((union.maxY - union.minY) - (tight.maxY - tight.minY)) +
      ((union.maxZ - union.minZ) - (tight.maxZ - tight.minZ))

  private def isVecInYZ(box: ShardSimpleCollisionBox, vec: ShardVector3dm | Null): Boolean =
    vec != null && vec.y >= box.minY && vec.y <= box.maxY && vec.z >= box.minZ && vec.z <= box.maxZ

  private def isVecInXZ(box: ShardSimpleCollisionBox, vec: ShardVector3dm | Null): Boolean =
    vec != null && vec.x >= box.minX && vec.x <= box.maxX && vec.z >= box.minZ && vec.z <= box.maxZ

  private def isVecInXY(box: ShardSimpleCollisionBox, vec: ShardVector3dm | Null): Boolean =
    vec != null && vec.x >= box.minX && vec.x <= box.maxX && vec.y >= box.minY && vec.y <= box.maxY

  def eyeHeightFor(player: ShardPlayer): Double =
    val tracking = player.tracking
    if tracking.swimming || tracking.gliding then SWIM_EYE_HEIGHT
    else if tracking.sneaking then SNEAK_EYE_HEIGHT
    else STAND_EYE_HEIGHT

  def aimErrorAxes(player: ShardPlayer, box: ShardSimpleCollisionBox, out: Array[Double]): Unit =
    out(0) = 0.0
    out(1) = 0.0
    val m = player.movement
    val dx = (box.minX + box.maxX) / 2 - m.x
    val dy = (box.minY + box.maxY) / 2 - (m.y + eyeHeightFor(player))
    val dz = (box.minZ + box.maxZ) / 2 - m.z
    val horizontal = math.sqrt(dx * dx + dz * dz)
    val distance = math.sqrt(dx * dx + dy * dy + dz * dz)
    if distance < MIN_AIM_DISTANCE then return

    var deltaYaw = m.yaw - Math.toDegrees(math.atan2(-dx, dz))
    deltaYaw -= math.floor(deltaYaw / FULL_TURN_DEGREES + HALF) * FULL_TURN_DEGREES
    val deltaPitch = m.pitch - Math.toDegrees(-math.atan2(dy, horizontal))

    val halfWidth = math.max(box.maxX - box.minX, box.maxZ - box.minZ) / 2
    val halfHeight = (box.maxY - box.minY) / 2
    val angularWidth = Math.toDegrees(math.atan2(halfWidth, distance))
    val angularHeight = Math.toDegrees(math.atan2(halfHeight, distance))
    if angularWidth > MIN_ANGULAR_EXTENT then out(0) = deltaYaw / angularWidth
    if angularHeight > MIN_ANGULAR_EXTENT then out(1) = deltaPitch / angularHeight

final class ShardTickData:
  import ShardTickData.*

  var sequenceId: Int = 0
  var tickIndex: Long = 0
  var dtMs: Float = 0f
  var targetEntityId: Int = -1

  var yaw: Float = 0f
  var pitch: Float = 0f

  var x: Double = 0.0
  var y: Double = 0.0
  var z: Double = 0.0
  var deltaY: Double = 0.0

  var movementBitfield: Short = 0

  var pose: Int = -1

  var inputForward: Boolean = false
  var inputBackward: Boolean = false
  var inputLeft: Boolean = false
  var inputRight: Boolean = false
  var inputJump: Boolean = false
  var inputShift: Boolean = false
  var inputSprint: Boolean = false
  var inputValid: Boolean = false

  var gameMode: Int = 0
  var foodLevel: Int = 20
  var health: Float = DEFAULT_HEALTH
  var damageTakenThisTick: Float = 0.0f
  var ticksSinceDamage: Int = 255
  var groundFriction: Float = 0.6f
  var fallDistance: Float = 0.0f
  var movementSpeed: Float = 0.1f

  var jumpAmplifier: Int = -1
  var slowFalling: Boolean = false
  var playerPing: Int = 0

  var speedAmplifier: Int = -1
  var slownessAmplifier: Int = -1
  var hasteAmplifier: Int = -1
  var miningFatigueAmplifier: Int = -1

  var entityInteractionRange: Float = 3.0f
  var scale: Float = 1.0f
  var gravity: Float = 0.08f
  var jumpStrength: Float = 0.42f
  var stepHeight: Float = 0.6f

  var targetValid: Boolean = false
  var targetDistance: Float = 0f
  var targetAimAngle: Float = 0f
  var targetRelX: Float = 0f
  var targetRelY: Float = 0f
  var targetRelZ: Float = 0f
  var ticksToAttack: Short = 0
  var attackThisTick: Boolean = false
  var aimError: Float = 0f
  var sameTargetAsPrevAttack: Boolean = false
  var targetsInRangeCount: Int = 0
  var isCritical: Boolean = false
  var raytraceHit: Boolean = false
  var attackRayOccluded: Boolean = false
  var raytraceMissDistance: Float = 0f
  var hitPointRelX: Float = 0f
  var hitPointRelY: Float = 0f
  var hitPointRelZ: Float = 0f

  var ticksSinceAttack: Int = 255
  var swungThisTick: Boolean = false
  var attackCooldownProgress: Float = 0f
  private var serverCooldownProgress: Float = 1f
  private val aimAxes: Array[Double] = new Array[Double](2)
  var attackSpeed: Float = 4.0f
  var isSprintingOnAttack: Boolean = false

  var receivedVelocityThisTick: Boolean = false
  var velocityX: Float = 0f
  var velocityY: Float = 0f
  var velocityZ: Float = 0f
  var ticksSinceVelocity: Int = 255
  var receivedExplosionThisTick: Boolean = false
  var ticksSinceExplosion: Int = 255
  var ticksSinceUseItem: Int = 255
  var activeItem: Short = 0

  var rotationPresent: Boolean = false
  var modeYaw: Float = 0f
  var modePitch: Float = 0f
  var modeYawValid: Boolean = false
  var modePitchValid: Boolean = false
  var deltaDotsYaw: Float = 0f
  var deltaDotsPitch: Float = 0f

  var activeFireworkCount: Int = 0
  var teleportTick: Boolean = false
  var exposureFraction: Float = 0f
  var boxInflation: Float = 0f

  var stuckMultX: Float = 1.0f
  var stuckMultY: Float = 1.0f
  var stuckMultZ: Float = 1.0f

  var onSlime: Boolean = false
  var onHoney: Boolean = false
  var onSoulSand: Boolean = false
  var onMud: Boolean = false

  var levitationAmplifier: Int = -1
  var swimFriction: Float = 0.8f
  var riptideActive: Boolean = false

  var windowStartKind: Short = START_MELEE_PLAYER
  var targetType: Short = TARGET_PLAYER

  var crystalSpawnToAttack: Short = NO_TICKS
  var useOffhand: Boolean = false
  var nearbyCrystalCount: Int = 0
  var anchorCharge: Short = NO_CHARGE
  var anchorUseInterval: Short = NO_TICKS

  var placeFace: Short = NO_FACE
  var placeBlockClass: Short = PLACE_NONE
  var placeBlockX: Int = 0
  var placeBlockY: Int = 0
  var placeBlockZ: Int = 0
  var placeCursorX: Float = NO_CURSOR
  var placeCursorY: Float = NO_CURSOR
  var placeCursorZ: Float = NO_CURSOR
  var placeInsideBlock: Boolean = false
  var placesThisTick: Short = 0

  var heldItemClass: Short = ITEM_UNKNOWN
  var offhandItemClass: Short = ITEM_UNKNOWN
  var slotSwitchesThisTick: Short = 0

  var explosionKbX: Float = 0f
  var explosionKbY: Float = 0f
  var explosionKbZ: Float = 0f

  var hittableEntitiesCount: Int = 0
  var aimErrorYaw: Float = 0f
  var aimErrorPitch: Float = 0f
  var raytraceDirsHitCount: Short = 0
  var raytraceHitStrict: Boolean = false

  def capture(player: ShardPlayer): Unit =
    val m = player.movement
    val tracking = player.tracking
    val aim = player.aim

    sequenceId = tracking.sequenceId
    tickIndex = tracking.tickIndex

    val nowMillis = System.currentTimeMillis()
    dtMs =
      if tracking.lastCaptureAtMillis == 0L then 0f
      else math.max(0L, math.min(DT_MS_MAX, nowMillis - tracking.lastCaptureAtMillis)).toFloat
    tracking.lastCaptureAtMillis = nowMillis
    targetEntityId = tracking.targetEntityId

    yaw = m.yaw
    pitch = m.pitch
    x = m.x
    y = m.y
    z = m.z

    teleportTick = !tracking.firstTickProcessed
    if tracking.firstTickProcessed then
      deltaY = m.y - tracking.lastY
    else
      deltaY = 0.0
      tracking.firstTickProcessed = true
      tracking.fallDistance = 0f
      aim.reset()
    tracking.lastY = m.y

    movementBitfield = tracking.buildMovementBitfield(player.packetStateData.packetPlayerOnGround)

    pose = tracking.pose

    inputForward = tracking.inputForward
    inputBackward = tracking.inputBackward
    inputLeft = tracking.inputLeft
    inputRight = tracking.inputRight
    inputJump = tracking.inputJump
    inputShift = tracking.inputShift
    inputSprint = tracking.inputSprint
    inputValid = tracking.inputValid

    rotationPresent = tracking.rotationThisTick

    activeItem = tracking.activeItem
    gameMode = tracking.gameMode
    foodLevel = tracking.foodLevel
    health = tracking.health
    damageTakenThisTick = tracking.damageTakenThisTick
    ticksSinceDamage = tracking.ticksSinceDamage
    groundFriction = tracking.groundFriction
    movementSpeed = tracking.movementSpeed

    updateFallDistance(player, tracking, deltaY)
    fallDistance = tracking.fallDistance

    jumpAmplifier = tracking.jumpAmplifier
    slowFalling = tracking.slowFalling
    playerPing = tracking.playerPing

    speedAmplifier = tracking.speedAmplifier
    slownessAmplifier = tracking.slownessAmplifier
    hasteAmplifier = tracking.hasteAmplifier
    miningFatigueAmplifier = tracking.miningFatigueAmplifier

    entityInteractionRange = tracking.entityInteractionRange
    scale = tracking.scale
    gravity = tracking.gravity
    jumpStrength = tracking.jumpStrength
    stepHeight = tracking.stepHeight

    captureAttackCooldown(player, tracking)
    captureCombatContext(player, tracking)

    attackThisTick = tracking.attackThisTick
    ticksSinceAttack = tracking.ticksSinceAttack
    swungThisTick = tracking.swungThisTick
    isSprintingOnAttack = tracking.isSprintingOnAttack
    attackSpeed = tracking.attackSpeed

    receivedVelocityThisTick = tracking.receivedVelocityThisTick
    if tracking.receivedVelocityThisTick then
      velocityX = tracking.velocityX
      velocityY = tracking.velocityY
      velocityZ = tracking.velocityZ
    else
      velocityX = 0f
      velocityY = 0f
      velocityZ = 0f
    ticksSinceVelocity = tracking.ticksSinceVelocity
    receivedExplosionThisTick = tracking.receivedExplosionThisTick
    ticksSinceExplosion = tracking.ticksSinceExplosion
    ticksSinceUseItem = tracking.ticksSinceUseItem

    modeYaw = aim.modeX.toFloat
    modePitch = aim.modeY.toFloat
    modeYawValid = aim.modeYawValid
    modePitchValid = aim.modePitchValid
    val dots = aim.consumeDots()
    deltaDotsYaw = if aim.modeYawValid then dots.yaw else 0f
    deltaDotsPitch = if aim.modePitchValid then dots.pitch else 0f

    activeFireworkCount = player.compensatedFireworks.count()

    stuckMultX = tracking.stuckMultX
    stuckMultY = tracking.stuckMultY
    stuckMultZ = tracking.stuckMultZ

    onSlime = tracking.onSlime
    onHoney = tracking.onHoney
    onSoulSand = tracking.onSoulSand
    onMud = tracking.onMud

    levitationAmplifier = tracking.levitationAmplifier
    swimFriction = computeSwimFriction(player, tracking)
    riptideActive = tracking.riptideActive

    captureCrystalContext(player, tracking)

  private def captureCrystalContext(player: ShardPlayer, tracking: ShardTrackingState): Unit =
    windowStartKind = START_MELEE_PLAYER

    crystalSpawnToAttack = tracking.crystalSpawnToAttack.toShort
    useOffhand = tracking.placeOffhand || (tracking.isUsingItem && tracking.usingOffhand)
    anchorCharge = tracking.anchorCharge.toShort
    anchorUseInterval = tracking.anchorUseInterval.toShort

    placeFace = tracking.placeFace.toShort
    placeBlockClass = tracking.placeBlockClass.toShort
    placeBlockX = tracking.placeBlockX
    placeBlockY = tracking.placeBlockY
    placeBlockZ = tracking.placeBlockZ
    placeCursorX = tracking.placeCursorX
    placeCursorY = tracking.placeCursorY
    placeCursorZ = tracking.placeCursorZ
    placeInsideBlock = tracking.placeInsideBlock
    placesThisTick = tracking.placesThisTick.toShort

    targetType = tracking.attackTargetType
    heldItemClass = classifyItem(
      if tracking.heldSlot >= 0 && tracking.heldSlot < tracking.hotbarItems.length then tracking.hotbarItems(tracking.heldSlot)
      else null
    )
    offhandItemClass = classifyItem(tracking.offhandItem)
    slotSwitchesThisTick = tracking.slotSwitchesThisTick.toShort

    if tracking.receivedExplosionThisTick then
      explosionKbX = tracking.explosionKbX
      explosionKbY = tracking.explosionKbY
      explosionKbZ = tracking.explosionKbZ
    else
      explosionKbX = 0f
      explosionKbY = 0f
      explosionKbZ = 0f

    var crystals = 0
    var hittable = 0
    val iter = player.compensatedEntities.entityMap.values().iterator()
    while iter.hasNext do
      val entity = iter.next()
      if entity.canHit() then
        val pos = entity.trackedServerPosition.pos
        val dx = pos.getX - player.movement.x
        val dy = pos.getY - player.movement.y
        val dz = pos.getZ - player.movement.z
        val distSq = dx * dx + dy * dy + dz * dz
        if distSq <= TARGETS_IN_RANGE_RADIUS_SQ then hittable += 1
        if entity.tpe == EntityTypes.END_CRYSTAL && distSq <= CRYSTAL_RADIUS_SQ then crystals += 1
    nearbyCrystalCount = crystals
    hittableEntitiesCount = hittable

  private def computeSwimFriction(player: ShardPlayer, tracking: ShardTrackingState): Float =
    val isNew = player.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_13)
    var friction = if tracking.sprinting && isNew then SWIM_FRICTION_SPRINT else SWIM_FRICTION_BASE
    var efficiency = tracking.waterMovementEfficiency
    if !player.packetStateData.packetPlayerOnGround then efficiency *= 0.5f
    if efficiency > 0f then friction += (DEPTH_STRIDER_FRICTION - friction) * efficiency
    if tracking.dolphinsGrace then friction = DOLPHINS_GRACE_FRICTION
    friction

  private def captureCombatContext(player: ShardPlayer, tracking: ShardTrackingState): Unit =
    val targetId = tracking.targetEntityId
    val entity: ShardPacketEntity | Null =
      if targetId >= 0 then player.compensatedEntities.getEntity(targetId)
      else null

    if entity != null && entity.canHit() then
      targetValid = true
      sameTargetAsPrevAttack = tracking.targetEntityId == tracking.lastTargetEntityId

      val targetBox = entity.getPossibleCollisionBoxes()
      targetDistance = ShardReachUtils.getMinReachToBox(player, targetBox).toFloat
      targetAimAngle = ShardReachUtils.getMinAngleToBox(player, targetBox).toFloat

      val targetPos = entity.trackedServerPosition.pos
      targetRelX = (targetPos.getX - player.movement.x).toFloat
      targetRelY = (targetPos.getY - player.movement.y).toFloat
      targetRelZ = (targetPos.getZ - player.movement.z).toFloat

      val tightBox = ShardGetBoundingBox.getPacketEntityBoundingBox(targetPos.getX, targetPos.getY, targetPos.getZ, entity)
      boxInflation = math.max(0f, ShardReachUtils.boxInflationExtent(targetBox, tightBox).toFloat)
      aimError = ShardReachUtils.aimError(player, tightBox).toFloat

      if tracking.attackThisTick then
        val hitPointUnion = computeRaytraceHitPoint(player, targetBox)
        raytraceHit = hitPointUnion != null
        val hitPointTight = computeRaytraceHitPoint(player, tightBox)
        raytraceHitStrict = hitPointTight != null
        raytraceDirsHitCount = countRaytraceHits(player, targetBox).toShort
        ShardReachUtils.aimErrorAxes(player, tightBox, aimAxes)
        aimErrorYaw = aimAxes(0).toFloat
        aimErrorPitch = aimAxes(1).toFloat
        raytraceMissDistance =
          if hitPointTight != null then 0f else computeRaytraceMissDistance(player, tightBox).toFloat
        if hitPointTight != null then
          val boxCenterX = (tightBox.minX + tightBox.maxX) * 0.5
          val boxCenterY = (tightBox.minY + tightBox.maxY) * 0.5
          val boxCenterZ = (tightBox.minZ + tightBox.maxZ) * 0.5
          hitPointRelX = (hitPointTight.x - boxCenterX).toFloat
          hitPointRelY = (hitPointTight.y - boxCenterY).toFloat
          hitPointRelZ = (hitPointTight.z - boxCenterZ).toFloat
        else
          hitPointRelX = 0f
          hitPointRelY = 0f
          hitPointRelZ = 0f
        isCritical = computeIsCritical(player, tracking)
        attackRayOccluded = computeAttackRayOccluded(player, targetBox)
        exposureFraction = ShardReachUtils.exposureFraction(player, tightBox)
      else
        raytraceHit = false
        raytraceMissDistance = 0f
        hitPointRelX = 0f
        hitPointRelY = 0f
        hitPointRelZ = 0f
        isCritical = false
        attackRayOccluded = false
        exposureFraction = 0f
        raytraceHitStrict = false
        raytraceDirsHitCount = 0
        aimErrorYaw = 0f
        aimErrorPitch = 0f

      targetsInRangeCount = countTargetsInRange(player)
    else
      targetValid = false
      sameTargetAsPrevAttack = false
      targetDistance = 0f
      targetAimAngle = 0f
      targetRelX = 0f
      targetRelY = 0f
      targetRelZ = 0f
      raytraceHit = false
      raytraceMissDistance = 0f
      hitPointRelX = 0f
      hitPointRelY = 0f
      hitPointRelZ = 0f
      isCritical = false
      attackRayOccluded = false
      exposureFraction = 0f
      boxInflation = 0f
      aimError = 0f
      targetsInRangeCount = 0
      raytraceHitStrict = false
      raytraceDirsHitCount = 0
      aimErrorYaw = 0f
      aimErrorPitch = 0f

  private def computeAttackRayOccluded(player: ShardPlayer, targetBox: ShardSimpleCollisionBox): Boolean =
    ShardReachUtils.isRayOccluded(
      player,
      ShardReachUtils.eyeOrigin(player),
      (targetBox.minX + targetBox.maxX) * 0.5,
      (targetBox.minY + targetBox.maxY) * 0.5,
      (targetBox.minZ + targetBox.maxZ) * 0.5,
    )

  private def captureAttackCooldown(player: ShardPlayer, tracking: ShardTrackingState): Unit =
    val isLegacyCombat = !modernCombatServer || player.clientVersion.isOlderThan(ClientVersion.V_1_9)
    serverCooldownProgress =
      if tracking.attackThisTick then tracking.preAttackCooldownProgress
      else
        val cooldownPeriod = TICKS_PER_SECOND / tracking.attackSpeed
        math.max(0f, math.min(1f, (tracking.cooldownTicks + 0.5f) / cooldownPeriod))
    attackCooldownProgress = if isLegacyCombat then 0f else serverCooldownProgress

  private def countRaytraceHits(player: ShardPlayer, targetBox: ShardSimpleCollisionBox): Int =
    val maxDistance = raytraceMaxDistance(player)
    val m = player.movement
    val lookDirs = ShardReachUtils.getPossibleLookDirs(player)
    val eyeHeights = ShardReachUtils.getPossibleEyeHeights()
    var hits = 0
    var dIdx = 0
    while dIdx < lookDirs.length do
      val lookVec = lookDirs(dIdx)
      var eIdx = 0
      while eIdx < eyeHeights.length do
        val eyeHeight = eyeHeights(eIdx)
        val origin = ShardVector3dm(m.x, m.y + eyeHeight, m.z)
        if ShardReachUtils.isVecInside(targetBox, origin) then hits += 1
        else
          val end = ShardVector3dm(
            origin.x + lookVec.x * maxDistance,
            origin.y + lookVec.y * maxDistance,
            origin.z + lookVec.z * maxDistance,
          )
          if ShardReachUtils.calculateIntercept(targetBox, origin, end) != null then hits += 1
        eIdx += 1
      dIdx += 1
    hits

  private def computeRaytraceHitPoint(player: ShardPlayer, targetBox: ShardSimpleCollisionBox): ShardVector3dm | Null =
    val lookDirs = ShardReachUtils.getPossibleLookDirs(player)
    val eyeHeights = ShardReachUtils.getPossibleEyeHeights()
    val maxDistance = raytraceMaxDistance(player)
    val m = player.movement
    var dIdx = 0
    while dIdx < lookDirs.length do
      val lookVec = lookDirs(dIdx)
      var eIdx = 0
      while eIdx < eyeHeights.length do
        val eyeHeight = eyeHeights(eIdx)
        val origin = ShardVector3dm(m.x, m.y + eyeHeight, m.z)
        if ShardReachUtils.isVecInside(targetBox, origin) then return origin
        val end = ShardVector3dm(
          origin.x + lookVec.x * maxDistance,
          origin.y + lookVec.y * maxDistance,
          origin.z + lookVec.z * maxDistance,
        )
        val intercept = ShardReachUtils.calculateIntercept(targetBox, origin, end)
        if intercept != null then return intercept
        eIdx += 1
      dIdx += 1
    null

  private def computeRaytraceMissDistance(player: ShardPlayer, targetBox: ShardSimpleCollisionBox): Double =
    val lookDirs = ShardReachUtils.getPossibleLookDirs(player)
    val eyeHeights = ShardReachUtils.getPossibleEyeHeights()
    val maxDistance = raytraceMaxDistance(player)
    val m = player.movement
    var minDist = Double.MaxValue
    var dIdx = 0
    while dIdx < lookDirs.length do
      val lookVec = lookDirs(dIdx)
      var eIdx = 0
      while eIdx < eyeHeights.length do
        val eye = eyeHeights(eIdx)
        val origin = ShardVector3dm(m.x, m.y + eye, m.z)
        val d = ShardReachUtils.minDistanceRayToBox(origin, lookVec, maxDistance, targetBox)
        if d < minDist then minDist = d
        if minDist == 0.0 then return 0.0
        eIdx += 1
      dIdx += 1
    minDist

  private def raytraceMaxDistance(player: ShardPlayer): Double =
    math.max(RAYTRACE_MIN_RANGE, player.tracking.entityInteractionRange + RAYTRACE_RANGE_MARGIN)

  private def computeIsCritical(player: ShardPlayer, tracking: ShardTrackingState): Boolean =
    if serverCooldownProgress <= CRIT_COOLDOWN_THRESHOLD then false
    else if tracking.fallDistance <= 0f then false
    else if player.packetStateData.packetPlayerOnGround then false
    else if tracking.isClimbing then false
    else if tracking.inWater then false
    else if tracking.inVehicle then false
    else if tracking.sprinting then false
    else if tracking.hasBlindness then false
    else true

  private def classifyItem(item: ItemType | Null): Short =
    if item == null || item == ItemTypes.AIR then ITEM_EMPTY
    else
      val exact = classifyExactItem(item)
      if exact != ITEM_UNKNOWN then exact
      else if item.hasAttribute(ItemTypes.ItemAttribute.SWORD) || item.hasAttribute(ItemTypes.ItemAttribute.AXE) then ITEM_MELEE_WEAPON
      else if item.hasAttribute(ItemTypes.ItemAttribute.EDIBLE) then ITEM_CONSUMABLE
      else if item.getPlacedType != null then ITEM_PLACEABLE_BLOCK
      else ITEM_UNKNOWN

  private def classifyExactItem(item: ItemType): Short =
    if item == ItemTypes.END_CRYSTAL then ITEM_END_CRYSTAL
    else if item == ItemTypes.RESPAWN_ANCHOR then ITEM_RESPAWN_ANCHOR
    else if item == ItemTypes.GLOWSTONE then ITEM_GLOWSTONE
    else if item == ItemTypes.TOTEM_OF_UNDYING then ITEM_TOTEM
    else if item == ItemTypes.SHIELD then ITEM_SHIELD
    else if item == ItemTypes.OBSIDIAN || item == ItemTypes.CRYING_OBSIDIAN || item == ItemTypes.BEDROCK then ITEM_CRYSTAL_BASE
    else if item == ItemTypes.BOW || item == ItemTypes.CROSSBOW || item == ItemTypes.TRIDENT then ITEM_RANGED
    else if item == ItemTypes.ENDER_PEARL || item == ItemTypes.SNOWBALL || item == ItemTypes.EGG then ITEM_PROJECTILE
    else ITEM_UNKNOWN

  private def countTargetsInRange(player: ShardPlayer): Int =
    var count = 0
    val iter = player.compensatedEntities.entityMap.values().iterator()
    while iter.hasNext do
      val entity = iter.next()
      if entity.isLivingEntity && entity.canHit() then
        val pos = entity.trackedServerPosition.pos
        val dx = pos.getX - player.movement.x
        val dy = pos.getY - player.movement.y
        val dz = pos.getZ - player.movement.z
        if dx * dx + dy * dy + dz * dz <= TARGETS_IN_RANGE_RADIUS_SQ then count += 1
    count

object ShardTickData:
  final val START_MELEE_PLAYER: Short = 0
  final val START_MELEE_LIVING_OTHER: Short = 1
  final val START_ATTACK_END_CRYSTAL: Short = 2
  final val START_ATTACK_ENTITY_OTHER: Short = 3
  final val START_USE_RESPAWN_ANCHOR: Short = 4
  final val START_PLACE_END_CRYSTAL: Short = 5
  final val START_EXPLOSION_RECEIVED: Short = 6

  final val TARGET_PLAYER: Short = 0
  final val TARGET_LIVING_OTHER: Short = 1
  final val TARGET_END_CRYSTAL: Short = 2
  final val TARGET_VEHICLE: Short = 3
  final val TARGET_OTHER: Short = 4
  final val TARGET_UNKNOWN: Short = 5

  final val ITEM_UNKNOWN: Short = 0
  final val ITEM_EMPTY: Short = 1
  final val ITEM_MELEE_WEAPON: Short = 2
  final val ITEM_END_CRYSTAL: Short = 3
  final val ITEM_CRYSTAL_BASE: Short = 4
  final val ITEM_RESPAWN_ANCHOR: Short = 5
  final val ITEM_GLOWSTONE: Short = 6
  final val ITEM_TOTEM: Short = 7
  final val ITEM_PLACEABLE_BLOCK: Short = 8
  final val ITEM_RANGED: Short = 9
  final val ITEM_PROJECTILE: Short = 10
  final val ITEM_CONSUMABLE: Short = 11
  final val ITEM_SHIELD: Short = 12

  final val PLACE_NONE: Short = 0
  final val PLACE_OBSIDIAN: Short = 1
  final val PLACE_BEDROCK: Short = 2
  final val PLACE_RESPAWN_ANCHOR: Short = 3
  final val PLACE_OTHER_SOLID: Short = 4
  final val PLACE_UNKNOWN: Short = 5

  final val NO_TICKS: Short = -1
  final val NO_CHARGE: Short = -1
  final val NO_FACE: Short = -1
  inline val NO_CURSOR = -1f

  private lazy val modernCombatServer: Boolean =
    try PacketEvents.getAPI.getServerManager.getVersion.isNewerThanOrEquals(ServerVersion.V_1_9)
    catch case _: Throwable => true

  inline val DEFAULT_HEALTH = 20.0f
  private inline val SLOW_FALL_CUTOFF = -0.5
  private inline val SLOW_FALL_CLAMP = 1.0f

  private def updateFallDistance(player: ShardPlayer, tracking: ShardTrackingState, deltaY: Double): Unit =
    if player.packetStateData.packetPlayerOnGround || tracking.isClimbing || tracking.inWater then
      tracking.fallDistance = 0f
    else if deltaY < 0 then
      tracking.fallDistance += (-deltaY).toFloat
    if tracking.gliding && deltaY > SLOW_FALL_CUTOFF && tracking.fallDistance > SLOW_FALL_CLAMP then
      tracking.fallDistance = SLOW_FALL_CLAMP
    if tracking.slowFalling || tracking.levitationAmplifier >= 0 then
      tracking.fallDistance = 0f

  private inline val SWIM_FRICTION_BASE = 0.8f
  private inline val SWIM_FRICTION_SPRINT = 0.9f
  private inline val DEPTH_STRIDER_FRICTION = 0.546f
  private inline val DOLPHINS_GRACE_FRICTION = 0.96f
  private inline val TICKS_PER_SECOND = 20.0f
  private inline val CRYSTAL_RADIUS_SQ = 64.0
  private inline val RAYTRACE_RANGE_MARGIN = 3.0
  private inline val RAYTRACE_MIN_RANGE = 6.0
  private inline val CRIT_COOLDOWN_THRESHOLD = 0.9f
  private inline val TARGETS_IN_RANGE_RADIUS_SQ = 36.0
  private inline val DT_MS_MAX = 10_000L

final class ShardTickBuffer(private val cap: Int):
  import ShardTickBuffer.*

  private val buffer: Array[ShardTickData] = Array.fill(cap)(ShardTickData())
  private val seqs: Array[Long] = new Array[Long](cap)
  private var writeIndex = 0
  private var ticksWritten = 0L
  private var attackBufferIndex = -1
  private var nextSeq = 1L

  def capacity(): Int = cap

  def currentSlot(): ShardTickData = buffer(writeIndex)

  def capture(player: ShardPlayer): Unit =
    seqs(writeIndex) = nextSeq
    currentSlot().capture(player)

  def captureRaw(fill: ShardTickData => Unit): Unit =
    seqs(writeIndex) = nextSeq
    fill(currentSlot())

  def advance(): Unit =
    nextSeq += 1
    ticksWritten += 1
    writeIndex = (writeIndex + 1) % cap

  def currentSeq(): Long = nextSeq

  def oldestSeq(): Long = math.max(1L, nextSeq - cap + 1)

  private def slotOf(seq: Long): Int =
    Math.floorMod(writeIndex - (nextSeq - seq), cap.toLong).toInt

  def holds(from: Long, to: Long): Boolean =
    from >= 1 && from <= to &&
      to <= nextSeq &&
      from >= oldestSeq() &&
      seqs(slotOf(from)) == from &&
      seqs(slotOf(to)) == to

  def rowAt(seq: Long): ShardTickData =
    val slot = slotOf(seq)
    require(seq >= oldestSeq() && seq <= nextSeq && seqs(slot) == seq, s"row $seq is not resident")
    buffer(slot)

  def resizedTo(newCapacity: Int): ShardTickBuffer =
    val resized = ShardTickBuffer(newCapacity)
    val keep = math.min(cap.toLong, math.min(newCapacity.toLong, nextSeq)).toInt
    var back = keep - 1
    while back >= 0 do
      val src = Math.floorMod(writeIndex - back, cap)
      val dst = Math.floorMod(-back, newCapacity)
      copyTickData(buffer(src), resized.buffer(dst))
      resized.seqs(dst) = seqs(src)
      if src == attackBufferIndex then resized.attackBufferIndex = dst
      back -= 1
    resized.writeIndex = 0
    resized.ticksWritten = ticksWritten
    resized.nextSeq = nextSeq
    resized

  def resetForSession(): Unit =
    ticksWritten = 0
    attackBufferIndex = -1

  def currentWriteIndex(): Int = writeIndex
  def attackIndex(): Int = attackBufferIndex

  def markAttack(): Unit =
    attackBufferIndex = writeIndex

  def extractWindow(
      preWindow: Int,
      postWindow: Int,
      ticksSinceAttack: Int,
      attackIdx: Int = attackBufferIndex,
      windowStartKind: Short = ShardTickData.START_MELEE_PLAYER,
  ): ArrayList[ShardTickData] | Null =
    if attackIdx < 0 then return null
    if ticksSinceAttack >= cap then return null

    val actualPostTicks = math.min(ticksSinceAttack, postWindow)
    val availablePre =
      math.max(
        0,
        math.min(preWindow.toLong, math.min(ticksWritten - 1, (cap - ticksSinceAttack - 1).toLong)).toInt,
      )
    val expectedSize = availablePre + actualPostTicks
    if expectedSize == 0 then return null

    val actualStartIdx = (attackIdx - availablePre + cap) % cap
    val attackSeqId = buffer(attackIdx).sequenceId
    if attackSeqId == 0 then return null

    val window = ArrayList[ShardTickData](expectedSize)
    var i = 0
    while i < expectedSize do
      val bufIdx = (actualStartIdx + i) % cap
      val tick = buffer(bufIdx)
      if tick.sequenceId != attackSeqId then return null
      val copy = ShardTickData()
      copyTickData(tick, copy)
      copy.ticksToAttack = (i - availablePre).toShort
      copy.windowStartKind = windowStartKind
      window.add(copy)
      i += 1
    window

object ShardTickBuffer:
  def copyTickData(src: ShardTickData, dst: ShardTickData): Unit =
    dst.sequenceId = src.sequenceId
    dst.tickIndex = src.tickIndex
    dst.dtMs = src.dtMs
    dst.targetEntityId = src.targetEntityId
    dst.yaw = src.yaw
    dst.pitch = src.pitch
    dst.x = src.x
    dst.y = src.y
    dst.z = src.z
    dst.movementBitfield = src.movementBitfield
    dst.pose = src.pose
    dst.inputForward = src.inputForward
    dst.inputBackward = src.inputBackward
    dst.inputLeft = src.inputLeft
    dst.inputRight = src.inputRight
    dst.inputJump = src.inputJump
    dst.inputShift = src.inputShift
    dst.inputSprint = src.inputSprint
    dst.gameMode = src.gameMode
    dst.foodLevel = src.foodLevel
    dst.health = src.health
    dst.damageTakenThisTick = src.damageTakenThisTick
    dst.ticksSinceDamage = src.ticksSinceDamage
    dst.groundFriction = src.groundFriction
    dst.fallDistance = src.fallDistance
    dst.movementSpeed = src.movementSpeed
    dst.jumpAmplifier = src.jumpAmplifier
    dst.slowFalling = src.slowFalling
    dst.playerPing = src.playerPing
    dst.speedAmplifier = src.speedAmplifier
    dst.slownessAmplifier = src.slownessAmplifier
    dst.hasteAmplifier = src.hasteAmplifier
    dst.miningFatigueAmplifier = src.miningFatigueAmplifier
    dst.entityInteractionRange = src.entityInteractionRange
    dst.scale = src.scale
    dst.gravity = src.gravity
    dst.jumpStrength = src.jumpStrength
    dst.stepHeight = src.stepHeight
    dst.targetValid = src.targetValid
    dst.targetDistance = src.targetDistance
    dst.targetAimAngle = src.targetAimAngle
    dst.targetRelX = src.targetRelX
    dst.targetRelY = src.targetRelY
    dst.targetRelZ = src.targetRelZ
    dst.ticksToAttack = src.ticksToAttack
    dst.attackThisTick = src.attackThisTick
    dst.aimError = src.aimError
    dst.sameTargetAsPrevAttack = src.sameTargetAsPrevAttack
    dst.targetsInRangeCount = src.targetsInRangeCount
    dst.isCritical = src.isCritical
    dst.raytraceHit = src.raytraceHit
    dst.attackRayOccluded = src.attackRayOccluded
    dst.raytraceMissDistance = src.raytraceMissDistance
    dst.inputValid = src.inputValid
    dst.hitPointRelX = src.hitPointRelX
    dst.hitPointRelY = src.hitPointRelY
    dst.hitPointRelZ = src.hitPointRelZ
    dst.ticksSinceAttack = src.ticksSinceAttack
    dst.swungThisTick = src.swungThisTick
    dst.attackCooldownProgress = src.attackCooldownProgress
    dst.attackSpeed = src.attackSpeed
    dst.isSprintingOnAttack = src.isSprintingOnAttack
    dst.receivedVelocityThisTick = src.receivedVelocityThisTick
    dst.velocityX = src.velocityX
    dst.velocityY = src.velocityY
    dst.velocityZ = src.velocityZ
    dst.ticksSinceVelocity = src.ticksSinceVelocity
    dst.receivedExplosionThisTick = src.receivedExplosionThisTick
    dst.ticksSinceExplosion = src.ticksSinceExplosion
    dst.ticksSinceUseItem = src.ticksSinceUseItem
    dst.activeItem = src.activeItem
    dst.rotationPresent = src.rotationPresent
    dst.modeYaw = src.modeYaw
    dst.modePitch = src.modePitch
    dst.modeYawValid = src.modeYawValid
    dst.modePitchValid = src.modePitchValid
    dst.deltaDotsYaw = src.deltaDotsYaw
    dst.deltaDotsPitch = src.deltaDotsPitch
    dst.activeFireworkCount = src.activeFireworkCount
    dst.stuckMultX = src.stuckMultX
    dst.stuckMultY = src.stuckMultY
    dst.stuckMultZ = src.stuckMultZ
    dst.onSlime = src.onSlime
    dst.onHoney = src.onHoney
    dst.onSoulSand = src.onSoulSand
    dst.onMud = src.onMud
    dst.levitationAmplifier = src.levitationAmplifier
    dst.swimFriction = src.swimFriction
    dst.riptideActive = src.riptideActive
    dst.teleportTick = src.teleportTick
    dst.exposureFraction = src.exposureFraction
    dst.boxInflation = src.boxInflation
    dst.windowStartKind = src.windowStartKind
    dst.targetType = src.targetType
    dst.crystalSpawnToAttack = src.crystalSpawnToAttack
    dst.useOffhand = src.useOffhand
    dst.nearbyCrystalCount = src.nearbyCrystalCount
    dst.anchorCharge = src.anchorCharge
    dst.anchorUseInterval = src.anchorUseInterval
    dst.placeFace = src.placeFace
    dst.placeBlockClass = src.placeBlockClass
    dst.placeBlockX = src.placeBlockX
    dst.placeBlockY = src.placeBlockY
    dst.placeBlockZ = src.placeBlockZ
    dst.placeCursorX = src.placeCursorX
    dst.placeCursorY = src.placeCursorY
    dst.placeCursorZ = src.placeCursorZ
    dst.placeInsideBlock = src.placeInsideBlock
    dst.placesThisTick = src.placesThisTick
    dst.heldItemClass = src.heldItemClass
    dst.offhandItemClass = src.offhandItemClass
    dst.slotSwitchesThisTick = src.slotSwitchesThisTick
    dst.explosionKbX = src.explosionKbX
    dst.explosionKbY = src.explosionKbY
    dst.explosionKbZ = src.explosionKbZ
    dst.hittableEntitiesCount = src.hittableEntitiesCount
    dst.aimErrorYaw = src.aimErrorYaw
    dst.aimErrorPitch = src.aimErrorPitch
    dst.raytraceDirsHitCount = src.raytraceDirsHitCount
    dst.raytraceHitStrict = src.raytraceHitStrict

final class ShardAttackWindowTracker:
  @volatile var ticksSinceAttack: Int = -1

  private var attackIndex: Int = -1
  private var anchorBufferId: Int = 0
  private var windowStartKind: Short = 0

  def reset(): Unit =
    ticksSinceAttack = -1
    attackIndex = -1
    anchorBufferId = 0
    windowStartKind = 0

  def onTick(
      buffer: ShardTickBuffer,
      windowStart: Boolean,
      eventKind: Short,
      postWindow: Int,
  )(onWindowReady: (Int, Int, Short) => Unit): Unit =
    val bufferId = System.identityHashCode(buffer)

    if windowStart && ticksSinceAttack < 0 then
      ticksSinceAttack = 0
      attackIndex = buffer.attackIndex()
      anchorBufferId = bufferId
      windowStartKind = eventKind

    if ticksSinceAttack >= 0 && anchorBufferId != bufferId then
      reset()
      return

    if ticksSinceAttack < 0 then return
    ticksSinceAttack += 1

    if ticksSinceAttack >= postWindow then
      val ticks = ticksSinceAttack
      val anchoredIndex = attackIndex
      val kind = windowStartKind
      reset()
      onWindowReady(ticks, anchoredIndex, kind)
