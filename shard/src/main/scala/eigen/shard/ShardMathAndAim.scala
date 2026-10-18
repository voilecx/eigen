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

import java.util.{ArrayDeque, Queue}
import java.util.HashMap as JHashMap

import com.github.retrooper.packetevents.protocol.world.states.WrappedBlockState
import com.github.retrooper.packetevents.protocol.world.states.`type`.{StateType, StateTypes, StateValue}
import com.github.retrooper.packetevents.util.Vector3d

final case class ShardHeadRotation(var yaw: Float = 0f, var pitch: Float = 0f)

final class ShardRotationUpdate(
    var from: ShardHeadRotation,
    var to: ShardHeadRotation,
    var deltaYaw: Float,
    var deltaPitch: Float,
)

final class ShardVector3dm(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0):
  def distanceSquared(other: ShardVector3dm): Double = distanceSquared(other.x, other.y, other.z)

  def distanceSquared(ox: Double, oy: Double, oz: Double): Double =
    val dx = x - ox
    val dy = y - oy
    val dz = z - oz
    dx * dx + dy * dy + dz * dz

  def distance(ox: Double, oy: Double, oz: Double): Double = math.sqrt(distanceSquared(ox, oy, oz))

  def toVector3d(): Vector3d = Vector3d(x, y, z)

  override def toString: String = s"Vector3dm($x, $y, $z)"

final class ShardSimpleCollisionBox(
    var minX: Double = 0.0,
    var minY: Double = 0.0,
    var minZ: Double = 0.0,
    var maxX: Double = 0.0,
    var maxY: Double = 0.0,
    var maxZ: Double = 0.0,
):
  def expand(x: Double, y: Double, z: Double): ShardSimpleCollisionBox =
    minX -= x
    minY -= y
    minZ -= z
    maxX += x
    maxY += y
    maxZ += z
    sort()

  def expand(value: Double): ShardSimpleCollisionBox =
    minX -= value
    minY -= value
    minZ -= value
    maxX += value
    maxY += value
    maxZ += value
    this

  def sort(): ShardSimpleCollisionBox =
    val sMinX = math.min(minX, maxX)
    val sMinY = math.min(minY, maxY)
    val sMinZ = math.min(minZ, maxZ)
    val sMaxX = math.max(minX, maxX)
    val sMaxY = math.max(minY, maxY)
    val sMaxZ = math.max(minZ, maxZ)
    minX = sMinX
    minY = sMinY
    minZ = sMinZ
    maxX = sMaxX
    maxY = sMaxY
    maxZ = sMaxZ
    this

  def copy(): ShardSimpleCollisionBox = ShardSimpleCollisionBox(minX, minY, minZ, maxX, maxY, maxZ)

  def distanceX(x: Double): Double =
    if x >= minX && x <= maxX then 0.0 else math.min(math.abs(x - minX), math.abs(x - maxX))

  def distanceY(y: Double): Double =
    if y >= minY && y <= maxY then 0.0 else math.min(math.abs(y - minY), math.abs(y - maxY))

  def distanceZ(z: Double): Double =
    if z >= minZ && z <= maxZ then 0.0 else math.min(math.abs(z - minZ), math.abs(z - maxZ))

object ShardSimpleCollisionBox:
  def combine(a: ShardSimpleCollisionBox, b: ShardSimpleCollisionBox): ShardSimpleCollisionBox =
    ShardSimpleCollisionBox(
      math.min(a.minX, b.minX),
      math.min(a.minY, b.minY),
      math.min(a.minZ, b.minZ),
      math.max(a.maxX, b.maxX),
      math.max(a.maxY, b.maxY),
      math.max(a.maxZ, b.maxZ),
    )

object ShardVanillaMath:
  private inline val SIN_SCALE_LEGACY = 10430.378f
  private inline val SIN_SCALE = 10430.378350470453
  private inline val COS_OFFSET = 16384
  private inline val TABLE_MASK = 65535
  private inline val TABLE_SIZE = 65536

  private val SIN: Array[Float] = Array.tabulate(TABLE_SIZE)(i => Math.sin(i / SIN_SCALE).toFloat)

  def sin(radians: Float, doubleIndex: Boolean): Float = SIN(index(radians, 0.0, doubleIndex))

  def cos(radians: Float, doubleIndex: Boolean): Float =
    SIN(index(radians, COS_OFFSET.toDouble, doubleIndex))

  private def index(radians: Float, offset: Double, doubleIndex: Boolean): Int =
    if doubleIndex then ((radians * SIN_SCALE + offset).toLong & TABLE_MASK.toLong).toInt
    else (radians * SIN_SCALE_LEGACY + offset.toFloat).toInt & TABLE_MASK

object ShardMath:
  private val MINIMUM_DIVISOR: Double = Math.pow(0.2f.toDouble, 3.0) * 8 * 0.15 - 1e-3

  def getMinimumDivisor(): Double = MINIMUM_DIVISOR

  def gcd(aInput: Double, bInput: Double): Double =
    if aInput == 0.0 then return 0.0
    var a = aInput
    var b = bInput
    if a < b then
      val temp = a
      a = b
      b = temp
    while b > MINIMUM_DIVISOR do
      val temp = a - (math.floor(a / b) * b)
      a = b
      b = temp
    a

final class ShardRunningMode(val maxSize: Int):
  require(maxSize != 0, "There's no mode to a size 0 list!")
  private val addList: Queue[java.lang.Double] = ArrayDeque[java.lang.Double](maxSize)
  private val popularityMap: JHashMap[java.lang.Double, java.lang.Integer] = JHashMap()

  var modeValue: Double = 0.0
  var modeCount: Int = 0

  def size(): Int = addList.size()

  def add(value: Double, bucketWidth: Double = ShardRunningMode.THRESHOLD): Unit =
    pop()
    val iter = popularityMap.entrySet().iterator()
    while iter.hasNext do
      val entry = iter.next()
      val k = entry.getKey.doubleValue()
      if math.abs(k - value) < bucketWidth then
        entry.setValue(entry.getValue.intValue() + 1)
        addList.add(entry.getKey)
        return
    popularityMap.put(value, 1)
    addList.add(value)

  def decay(): Unit =
    val tpe = addList.poll()
    if tpe == null then return
    val popularity = popularityMap.getOrDefault(tpe, 0).intValue()
    if popularity == 1 then popularityMap.remove(tpe)
    else popularityMap.put(tpe, popularity - 1)

  private def pop(): Unit =
    if addList.size() >= maxSize then decay()

  def clear(): Unit =
    addList.clear()
    popularityMap.clear()
    modeValue = 0.0
    modeCount = 0

  def updateMode(): Unit =
    var max = 0
    var mostPopular = 0.0
    val iter = popularityMap.entrySet().iterator()
    while iter.hasNext do
      val entry = iter.next()
      val v = entry.getValue.intValue()
      if v > max then
        max = v
        mostPopular = entry.getKey.doubleValue()
    modeValue = mostPopular
    modeCount = max

object ShardRunningMode:
  inline val THRESHOLD = 1e-3

final class ShardAimProcessor:
  import ShardAimProcessor.*

  var modeX: Double = 0.0
  var modeY: Double = 0.0
  var deltaDotsX: Double = 0.0
  var deltaDotsY: Double = 0.0

  private val xRotMode = ShardRunningMode(TOTAL_SAMPLES_THRESHOLD)
  private val yRotMode = ShardRunningMode(TOTAL_SAMPLES_THRESHOLD)
  private var lastXRot = 0.0
  private var lastYRot = 0.0
  private var dotsFresh = false

  def modeYawValid: Boolean = xRotMode.modeCount > SIGNIFICANT_SAMPLES_THRESHOLD
  def modePitchValid: Boolean = yRotMode.modeCount > SIGNIFICANT_SAMPLES_THRESHOLD

  def process(rotationUpdate: ShardRotationUpdate): Unit =
    val deltaYawAbs = math.abs(rotationUpdate.deltaYaw).toDouble
    val deltaPitchAbs = math.abs(rotationUpdate.deltaPitch).toDouble

    val divisorX = ShardMath.gcd(deltaYawAbs, lastXRot)
    if deltaYawAbs > 0 && deltaYawAbs < MAX_SAMPLE_DELTA && divisorX > ShardMath.getMinimumDivisor() then
      val yawUlp = math.ulp(math.max(math.abs(rotationUpdate.from.yaw), math.abs(rotationUpdate.to.yaw))).toDouble
      if yawUlp < divisorX * YAW_ULP_DIVISOR_FRACTION then
        if lastXRot != 0.0 then
          xRotMode.add(divisorX, math.max(ShardRunningMode.THRESHOLD, yawUlp * YAW_BUCKET_ULP_FACTOR))
        lastXRot = deltaYawAbs

    val divisorY = ShardMath.gcd(deltaPitchAbs, lastYRot)
    if deltaPitchAbs > 0 && deltaPitchAbs < MAX_SAMPLE_DELTA && divisorY > ShardMath.getMinimumDivisor() then
      if lastYRot != 0.0 then yRotMode.add(divisorY)
      lastYRot = deltaPitchAbs

    if xRotMode.size() > SIGNIFICANT_SAMPLES_THRESHOLD then
      xRotMode.updateMode()
      if xRotMode.modeCount > SIGNIFICANT_SAMPLES_THRESHOLD then modeX = xRotMode.modeValue

    if yRotMode.size() > SIGNIFICANT_SAMPLES_THRESHOLD then
      yRotMode.updateMode()
      if yRotMode.modeCount > SIGNIFICANT_SAMPLES_THRESHOLD then modeY = yRotMode.modeValue

    if modeX > 0 then deltaDotsX = deltaYawAbs / modeX
    if modeY > 0 then deltaDotsY = deltaPitchAbs / modeY

    dotsFresh = true

  def consumeDots(): ShardHeadRotation =
    val dots =
      if dotsFresh then ShardHeadRotation(deltaDotsX.toFloat, deltaDotsY.toFloat)
      else ShardHeadRotation(0f, 0f)
    dotsFresh = false
    dots

  def reset(): Unit =
    xRotMode.clear()
    yRotMode.clear()
    lastXRot = 0.0
    lastYRot = 0.0
    modeX = 0.0
    modeY = 0.0
    deltaDotsX = 0.0
    deltaDotsY = 0.0
    dotsFresh = false

object ShardAimProcessor:
  inline val SIGNIFICANT_SAMPLES_THRESHOLD = 15
  inline val TOTAL_SAMPLES_THRESHOLD = 80
  inline val MAX_SAMPLE_DELTA = 5.0
  inline val YAW_ULP_DIVISOR_FRACTION = 1.0 / 16.0
  inline val YAW_BUCKET_ULP_FACTOR = 2.0

object ShardBlockFriction:
  inline val DEFAULT = 0.6f

  def of(tpe: StateType | Null): Float =
    if tpe == StateTypes.ICE || tpe == StateTypes.PACKED_ICE || tpe == StateTypes.FROSTED_ICE then 0.98f
    else if tpe == StateTypes.BLUE_ICE then 0.989f
    else if tpe == StateTypes.SLIME_BLOCK || tpe == StateTypes.HONEY_BLOCK then 0.8f
    else DEFAULT

  def isClimbable(tpe: StateType | Null): Boolean =
    tpe == StateTypes.LADDER ||
      tpe == StateTypes.VINE ||
      tpe == StateTypes.TWISTING_VINES ||
      tpe == StateTypes.TWISTING_VINES_PLANT ||
      tpe == StateTypes.WEEPING_VINES ||
      tpe == StateTypes.WEEPING_VINES_PLANT ||
      tpe == StateTypes.CAVE_VINES ||
      tpe == StateTypes.CAVE_VINES_PLANT ||
      tpe == StateTypes.SCAFFOLDING

  def stuckMultiplier(tpe: StateType | Null): Array[Float] | Null =
    if tpe == StateTypes.COBWEB then Array(0.25f, 0.05f, 0.25f)
    else if tpe == StateTypes.SWEET_BERRY_BUSH then Array(0.8f, 0.75f, 0.8f)
    else if tpe == StateTypes.POWDER_SNOW then Array(0.9f, 1.5f, 0.9f)
    else null

  def isSlime(tpe: StateType | Null): Boolean = tpe == StateTypes.SLIME_BLOCK
  def isHoney(tpe: StateType | Null): Boolean = tpe == StateTypes.HONEY_BLOCK
  def isSoulSand(tpe: StateType | Null): Boolean = tpe == StateTypes.SOUL_SAND
  def isMud(tpe: StateType | Null): Boolean = tpe == StateTypes.MUD

  private val WATER_HOLDING_BLOCKS: Set[StateType] = Set(
    StateTypes.KELP,
    StateTypes.KELP_PLANT,
    StateTypes.SEAGRASS,
    StateTypes.TALL_SEAGRASS,
    StateTypes.BUBBLE_COLUMN,
  )

  def isWater(state: WrappedBlockState | Null): Boolean =
    if state == null then false
    else if state.getType == StateTypes.WATER || WATER_HOLDING_BLOCKS.contains(state.getType) then true
    else state.hasProperty(StateValue.WATERLOGGED) && state.isWaterlogged
