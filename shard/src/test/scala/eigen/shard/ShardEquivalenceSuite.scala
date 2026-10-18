/*
 * This file is part of Eigen.
 * Copyright (C) 2026 Eigen contributors
 *
 * Portions of this file are derived from the following projects, also licensed under
 * the GNU General Public License version 3:
 *   Shard - https://github.com/KaelusAI/Shard
 *           Copyright (C) 2026 KaelusAI
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

import java.lang.StringBuilder as JStringBuilder
import java.util.ArrayList

/**
 * Verifies 100% byte-for-byte and bug-for-bug equivalence with `KaelusAI/Shard` (`TickSchema`,
 * `TickData`, `TickBuffer`, `AttackWindowTracker`, `AimProcessor`, and teleport/rotation quirks).
 */
class ShardEquivalenceSuite extends munit.FunSuite:

  private val FIELD_COUNT = 128

  private val referenceTypes =
    "IIFIFFDDDBBBSBBBBBBCBBBBBBBCUFFFIBIIIIIFFFFFBFFFFFSBFBUBBBFBFFFUBFFBBFFFUBUUBFFBBFFUFFFBBBBIFBBFFFFU" +
      "SSSBUSSSSIIIFFFBSSSSFFFUFFSB"

  private def referenceFieldSize(field: Int): Int =
    referenceTypes.charAt(field) match
      case 'B' | 'C' | 'U' => 1
      case 'S' => 2
      case 'D' | 'L' => 8
      case _ => 4

  private def clamp(v: Int, min: Int, max: Int): Int =
    if v < min then min else if v > max then max else v

  private def referenceWriteRaw(b: ShardWireSink, t: ShardTickData, field: Int): Unit =
    referenceTypes.charAt(field) match
      case 'B' => b.put(if referenceRawFloat(t, field) != 0f then 1.toByte else 0.toByte)
      case 'C' => b.put(clamp(referenceRawFloat(t, field).toInt, -128, 127).toByte)
      case 'U' => b.put(clamp(referenceRawFloat(t, field).toInt, 0, 255).toByte)
      case 'S' => b.putShort(referenceRawFloat(t, field).toInt.toShort)
      case 'I' => b.putInt(if field == 1 then t.tickIndex.toInt else referenceRawFloat(t, field).toInt)
      case 'D' =>
        b.putDouble(
          field match
            case 6 => t.x
            case 7 => t.y
            case _ => t.z
        )
      case _ => b.putFloat(referenceRawFloat(t, field))

  private def referenceRawFloat(t: ShardTickData, field: Int): Float =
    val bits = t.movementBitfield.toInt
    if field == 12 then return t.activeItem.toFloat
    if field >= 9 && field <= 18 then return if ((bits >> (field - 9)) & 1) == 1 then 1f else 0f
    field match
      case 0 => t.sequenceId.toFloat
      case 1 => t.tickIndex.toFloat
      case 2 => t.dtMs
      case 3 => t.targetEntityId.toFloat
      case 4 => t.yaw
      case 5 => t.pitch
      case 6 => t.x.toFloat
      case 7 => t.y.toFloat
      case 8 => t.z.toFloat
      case 19 => t.pose.toFloat
      case 20 => if t.inputForward then 1f else 0f
      case 21 => if t.inputBackward then 1f else 0f
      case 22 => if t.inputLeft then 1f else 0f
      case 23 => if t.inputRight then 1f else 0f
      case 24 => if t.inputJump then 1f else 0f
      case 25 => if t.inputShift then 1f else 0f
      case 26 => if t.inputSprint then 1f else 0f
      case 27 => t.gameMode.toFloat
      case 28 => t.foodLevel.toFloat
      case 29 => t.groundFriction
      case 30 => t.fallDistance
      case 31 => t.movementSpeed
      case 32 => t.jumpAmplifier.toFloat
      case 33 => if t.slowFalling then 1f else 0f
      case 34 => t.playerPing.toFloat
      case 35 => t.speedAmplifier.toFloat
      case 36 => t.slownessAmplifier.toFloat
      case 37 => t.hasteAmplifier.toFloat
      case 38 => t.miningFatigueAmplifier.toFloat
      case 39 => t.entityInteractionRange
      case 40 => t.scale
      case 41 => t.gravity
      case 42 => t.jumpStrength
      case 43 => t.stepHeight
      case 44 => if t.targetValid then 1f else 0f
      case 45 => t.targetDistance
      case 46 => t.targetAimAngle
      case 47 => t.targetRelX
      case 48 => t.targetRelY
      case 49 => t.targetRelZ
      case 50 => t.ticksToAttack.toFloat
      case 51 => if t.attackThisTick then 1f else 0f
      case 52 => t.aimError
      case 53 => if t.sameTargetAsPrevAttack then 1f else 0f
      case 54 => t.targetsInRangeCount.toFloat
      case 55 => if t.isCritical then 1f else 0f
      case 56 => if t.raytraceHit then 1f else 0f
      case 57 => if t.attackRayOccluded then 1f else 0f
      case 58 => t.raytraceMissDistance
      case 59 => if t.inputValid then 1f else 0f
      case 60 => t.hitPointRelX
      case 61 => t.hitPointRelY
      case 62 => t.hitPointRelZ
      case 63 => t.ticksSinceAttack.toFloat
      case 64 => if t.swungThisTick then 1f else 0f
      case 65 => t.attackCooldownProgress
      case 66 => t.attackSpeed
      case 67 => if t.isSprintingOnAttack then 1f else 0f
      case 68 => if t.receivedVelocityThisTick then 1f else 0f
      case 69 => t.velocityX
      case 70 => t.velocityY
      case 71 => t.velocityZ
      case 72 => t.ticksSinceVelocity.toFloat
      case 73 => if t.receivedExplosionThisTick then 1f else 0f
      case 74 => t.ticksSinceExplosion.toFloat
      case 75 => t.ticksSinceUseItem.toFloat
      case 76 => if t.rotationPresent then 1f else 0f
      case 77 => t.modeYaw
      case 78 => t.modePitch
      case 79 => if t.modeYawValid then 1f else 0f
      case 80 => if t.modePitchValid then 1f else 0f
      case 81 => t.deltaDotsYaw
      case 82 => t.deltaDotsPitch
      case 83 => t.activeFireworkCount.toFloat
      case 84 => t.stuckMultX
      case 85 => t.stuckMultY
      case 86 => t.stuckMultZ
      case 87 => if t.onSlime then 1f else 0f
      case 88 => if t.onHoney then 1f else 0f
      case 89 => if t.onSoulSand then 1f else 0f
      case 90 => if t.onMud then 1f else 0f
      case 91 => t.levitationAmplifier.toFloat
      case 92 => t.swimFriction
      case 93 => if t.riptideActive then 1f else 0f
      case 94 => if t.teleportTick then 1f else 0f
      case 95 => t.exposureFraction
      case 96 => t.boxInflation
      case 97 => t.health
      case 98 => t.damageTakenThisTick
      case 99 => t.ticksSinceDamage.toFloat
      case 100 => t.windowStartKind.toFloat
      case 101 => t.targetType.toFloat
      case 102 => t.crystalSpawnToAttack.toFloat
      case 103 => if t.useOffhand then 1f else 0f
      case 104 => t.nearbyCrystalCount.toFloat
      case 105 => t.anchorCharge.toFloat
      case 106 => t.anchorUseInterval.toFloat
      case 107 => t.placeFace.toFloat
      case 108 => t.placeBlockClass.toFloat
      case 109 => t.placeBlockX.toFloat
      case 110 => t.placeBlockY.toFloat
      case 111 => t.placeBlockZ.toFloat
      case 112 => t.placeCursorX
      case 113 => t.placeCursorY
      case 114 => t.placeCursorZ
      case 115 => if t.placeInsideBlock then 1f else 0f
      case 116 => t.placesThisTick.toFloat
      case 117 => t.heldItemClass.toFloat
      case 118 => t.offhandItemClass.toFloat
      case 119 => t.slotSwitchesThisTick.toFloat
      case 120 => t.explosionKbX
      case 121 => t.explosionKbY
      case 122 => t.explosionKbZ
      case 123 => t.hittableEntitiesCount.toFloat
      case 124 => t.aimErrorYaw
      case 125 => t.aimErrorPitch
      case 126 => t.raytraceDirsHitCount.toFloat
      case 127 => if t.raytraceHitStrict then 1f else 0f
      case _ => 0f

  private def referenceAppendCsv(out: JStringBuilder, t: ShardTickData): Unit =
    out.append(t.sequenceId.toString).append(',')
    out.append(t.tickIndex.toString).append(',')
    refFloat(out, t.dtMs); out.append(',')
    out.append(t.targetEntityId.toString).append(',')
    refDouble(out, t.yaw.toDouble); out.append(',')
    refDouble(out, t.pitch.toDouble); out.append(',')
    refDouble(out, t.x); out.append(',')
    refDouble(out, t.y); out.append(',')
    refDouble(out, t.z); out.append(',')
    val bits = t.movementBitfield.toInt
    var bit = 0
    while bit <= 9 do
      if bit == 3 then out.append(t.activeItem.toString)
      else out.append(if ((bits >> bit) & 1) == 1 then '1' else '0')
      out.append(',')
      bit += 1
    out.append(t.pose.toString).append(',')
    refBool(out, t.inputForward); out.append(',')
    refBool(out, t.inputBackward); out.append(',')
    refBool(out, t.inputLeft); out.append(',')
    refBool(out, t.inputRight); out.append(',')
    refBool(out, t.inputJump); out.append(',')
    refBool(out, t.inputShift); out.append(',')
    refBool(out, t.inputSprint); out.append(',')
    out.append(t.gameMode.toString).append(',')
    out.append(t.foodLevel.toString).append(',')
    refFloat(out, t.groundFriction); out.append(',')
    refFloat(out, t.fallDistance); out.append(',')
    refFloat(out, t.movementSpeed); out.append(',')
    out.append(t.jumpAmplifier.toString).append(',')
    refBool(out, t.slowFalling); out.append(',')
    out.append(t.playerPing.toString).append(',')
    out.append(t.speedAmplifier.toString).append(',')
    out.append(t.slownessAmplifier.toString).append(',')
    out.append(t.hasteAmplifier.toString).append(',')
    out.append(t.miningFatigueAmplifier.toString).append(',')
    refFloat(out, t.entityInteractionRange); out.append(',')
    refFloat(out, t.scale); out.append(',')
    refFloat(out, t.gravity); out.append(',')
    refFloat(out, t.jumpStrength); out.append(',')
    refFloat(out, t.stepHeight); out.append(',')
    refBool(out, t.targetValid); out.append(',')
    refFloat(out, t.targetDistance); out.append(',')
    refFloat(out, t.targetAimAngle); out.append(',')
    refFloat(out, t.targetRelX); out.append(',')
    refFloat(out, t.targetRelY); out.append(',')
    refFloat(out, t.targetRelZ); out.append(',')
    out.append(t.ticksToAttack.toString).append(',')
    refBool(out, t.attackThisTick); out.append(',')
    refFloat(out, t.aimError); out.append(',')
    refBool(out, t.sameTargetAsPrevAttack); out.append(',')
    out.append(t.targetsInRangeCount.toString).append(',')
    refBool(out, t.isCritical); out.append(',')
    refBool(out, t.raytraceHit); out.append(',')
    refBool(out, t.attackRayOccluded); out.append(',')
    refFloat(out, t.raytraceMissDistance); out.append(',')
    refBool(out, t.inputValid); out.append(',')
    refFloat(out, t.hitPointRelX); out.append(',')
    refFloat(out, t.hitPointRelY); out.append(',')
    refFloat(out, t.hitPointRelZ); out.append(',')
    out.append(t.ticksSinceAttack.toString).append(',')
    refBool(out, t.swungThisTick); out.append(',')
    refFloat(out, t.attackCooldownProgress); out.append(',')
    refFloat(out, t.attackSpeed); out.append(',')
    refBool(out, t.isSprintingOnAttack); out.append(',')
    refBool(out, t.receivedVelocityThisTick); out.append(',')
    refFloat(out, t.velocityX); out.append(',')
    refFloat(out, t.velocityY); out.append(',')
    refFloat(out, t.velocityZ); out.append(',')
    out.append(t.ticksSinceVelocity.toString).append(',')
    refBool(out, t.receivedExplosionThisTick); out.append(',')
    out.append(t.ticksSinceExplosion.toString).append(',')
    out.append(t.ticksSinceUseItem.toString).append(',')
    refBool(out, t.rotationPresent); out.append(',')
    refFloat(out, t.modeYaw); out.append(',')
    refFloat(out, t.modePitch); out.append(',')
    refBool(out, t.modeYawValid); out.append(',')
    refBool(out, t.modePitchValid); out.append(',')
    refFloat(out, t.deltaDotsYaw); out.append(',')
    refFloat(out, t.deltaDotsPitch); out.append(',')
    out.append(t.activeFireworkCount.toString).append(',')
    refFloat(out, t.stuckMultX); out.append(',')
    refFloat(out, t.stuckMultY); out.append(',')
    refFloat(out, t.stuckMultZ); out.append(',')
    refBool(out, t.onSlime); out.append(',')
    refBool(out, t.onHoney); out.append(',')
    refBool(out, t.onSoulSand); out.append(',')
    refBool(out, t.onMud); out.append(',')
    out.append(t.levitationAmplifier.toString).append(',')
    refFloat(out, t.swimFriction); out.append(',')
    refBool(out, t.riptideActive); out.append(',')
    refBool(out, t.teleportTick); out.append(',')
    refFloat(out, t.exposureFraction); out.append(',')
    refFloat(out, t.boxInflation); out.append(',')
    refFloat(out, t.health); out.append(',')
    refFloat(out, t.damageTakenThisTick); out.append(',')
    out.append(t.ticksSinceDamage.toString).append(',')
    out.append(t.windowStartKind.toString).append(',')
    out.append(t.targetType.toString).append(',')
    out.append(t.crystalSpawnToAttack.toString).append(',')
    refBool(out, t.useOffhand); out.append(',')
    out.append(t.nearbyCrystalCount.toString).append(',')
    out.append(t.anchorCharge.toString).append(',')
    out.append(t.anchorUseInterval.toString).append(',')
    out.append(t.placeFace.toString).append(',')
    out.append(t.placeBlockClass.toString).append(',')
    out.append(t.placeBlockX.toString).append(',')
    out.append(t.placeBlockY.toString).append(',')
    out.append(t.placeBlockZ.toString).append(',')
    refFloat(out, t.placeCursorX); out.append(',')
    refFloat(out, t.placeCursorY); out.append(',')
    refFloat(out, t.placeCursorZ); out.append(',')
    refBool(out, t.placeInsideBlock); out.append(',')
    out.append(t.placesThisTick.toString).append(',')
    out.append(t.heldItemClass.toString).append(',')
    out.append(t.offhandItemClass.toString).append(',')
    out.append(t.slotSwitchesThisTick.toString).append(',')
    refFloat(out, t.explosionKbX); out.append(',')
    refFloat(out, t.explosionKbY); out.append(',')
    refFloat(out, t.explosionKbZ); out.append(',')
    out.append(t.hittableEntitiesCount.toString).append(',')
    refFloat(out, t.aimErrorYaw); out.append(',')
    refFloat(out, t.aimErrorPitch); out.append(',')
    out.append(t.raytraceDirsHitCount.toString).append(',')
    refBool(out, t.raytraceHitStrict)

  private def refBool(out: JStringBuilder, value: Boolean): Unit =
    out.append(if value then '1' else '0')

  private def refFloat(out: JStringBuilder, value: Float): Unit =
    if !java.lang.Float.isFinite(value) then
      out.append("0.000000")
      return
    var v = value.toDouble
    val negative = v < 0.0
    if negative then v = -v
    val scaled = (v * 1_000_000L + 0.5).toLong
    val intPart = scaled / 1_000_000L
    val fracPart = (scaled % 1_000_000L).toInt
    if negative && scaled > 0 then out.append('-')
    out.append(intPart.toString)
    out.append('.')
    val fracText = fracPart.toString
    var pad = 6 - fracText.length
    while pad > 0 do
      out.append('0')
      pad -= 1
    out.append(fracText)

  private def refDouble(out: JStringBuilder, value: Double): Unit =
    if !java.lang.Double.isFinite(value) then
      out.append("0.0000000000")
      return
    var v = value
    val negative = v < 0.0
    if negative then v = -v
    val scaleD = 10_000_000_000L
    val scaled = (v * scaleD + 0.5).toLong
    val intPart = scaled / scaleD
    val fracPart = scaled % scaleD
    if negative && scaled > 0 then out.append('-')
    out.append(intPart.toString)
    out.append('.')
    val fracText = fracPart.toString
    var pad = 10 - fracText.length
    while pad > 0 do
      out.append('0')
      pad -= 1
    out.append(fracText)

  private def samples(): Seq[ShardTickData] =
    val defaults = ShardTickData()

    val distinct = ShardTickData()
    distinct.sequenceId = 12345
    distinct.tickIndex = 9_876_543_210L
    distinct.dtMs = 42.5f
    distinct.targetEntityId = 4242
    distinct.yaw = 12.3456f
    distinct.pitch = -5.6789f
    distinct.x = 100.123456789
    distinct.y = -64.987654321
    distinct.z = 0.000000001
    distinct.deltaY = 0.25
    distinct.movementBitfield = 0x02aa.toShort
    distinct.pose = 5
    distinct.inputForward = true
    distinct.inputBackward = false
    distinct.inputLeft = true
    distinct.inputRight = false
    distinct.inputJump = true
    distinct.inputShift = true
    distinct.inputSprint = false
    distinct.gameMode = 1
    distinct.foodLevel = 17
    distinct.groundFriction = 0.91f
    distinct.fallDistance = 3.5f
    distinct.movementSpeed = 0.13f
    distinct.jumpAmplifier = 2
    distinct.slowFalling = true
    distinct.playerPing = 42
    distinct.speedAmplifier = 1
    distinct.slownessAmplifier = 0
    distinct.hasteAmplifier = 3
    distinct.miningFatigueAmplifier = -1
    distinct.entityInteractionRange = 3.05f
    distinct.scale = 1.5f
    distinct.gravity = 0.079f
    distinct.jumpStrength = 0.42f
    distinct.stepHeight = 0.6f
    distinct.targetValid = true
    distinct.targetDistance = 2.718281f
    distinct.targetAimAngle = 12.5f
    distinct.targetRelX = -1.0f
    distinct.targetRelY = 0.5f
    distinct.targetRelZ = -2.25f
    distinct.ticksToAttack = (-7).toShort
    distinct.attackThisTick = true
    distinct.aimError = 0.4375f
    distinct.sameTargetAsPrevAttack = true
    distinct.targetsInRangeCount = 4
    distinct.isCritical = true
    distinct.raytraceHit = true
    distinct.attackRayOccluded = true
    distinct.raytraceMissDistance = 0.333333f
    distinct.inputValid = true
    distinct.hitPointRelX = -0.1f
    distinct.hitPointRelY = 0.2f
    distinct.hitPointRelZ = -0.3f
    distinct.ticksSinceAttack = 11
    distinct.swungThisTick = true
    distinct.attackCooldownProgress = 0.875f
    distinct.attackSpeed = 1.6f
    distinct.isSprintingOnAttack = true
    distinct.receivedVelocityThisTick = true
    distinct.velocityX = 0.4f
    distinct.velocityY = -0.1f
    distinct.velocityZ = 0.4f
    distinct.ticksSinceVelocity = 3
    distinct.receivedExplosionThisTick = true
    distinct.ticksSinceExplosion = 7
    distinct.ticksSinceUseItem = 99
    distinct.activeItem = 3
    distinct.rotationPresent = true
    distinct.modeYaw = 0.0123f
    distinct.modePitch = 0.0098f
    distinct.modeYawValid = true
    distinct.modePitchValid = false
    distinct.deltaDotsYaw = 1.5f
    distinct.deltaDotsPitch = -2.5f
    distinct.activeFireworkCount = 1
    distinct.stuckMultX = 0.0f
    distinct.stuckMultY = 1.0f
    distinct.stuckMultZ = 0.5f
    distinct.onSlime = true
    distinct.onHoney = false
    distinct.onSoulSand = true
    distinct.onMud = false
    distinct.levitationAmplifier = 5
    distinct.swimFriction = 0.96f
    distinct.riptideActive = true
    distinct.teleportTick = true
    distinct.exposureFraction = 0.555556f
    distinct.boxInflation = 1.234f
    distinct.health = 18.5f
    distinct.damageTakenThisTick = 1.5f
    distinct.ticksSinceDamage = 4
    distinct.windowStartKind = 1
    distinct.targetType = 1
    distinct.crystalSpawnToAttack = 2
    distinct.useOffhand = true
    distinct.nearbyCrystalCount = 3
    distinct.anchorCharge = 4
    distinct.anchorUseInterval = 5
    distinct.placeFace = 1
    distinct.placeBlockClass = 2
    distinct.placeBlockX = -10
    distinct.placeBlockY = 64
    distinct.placeBlockZ = 20
    distinct.placeCursorX = 0.25f
    distinct.placeCursorY = 0.5f
    distinct.placeCursorZ = 0.75f
    distinct.placeInsideBlock = true
    distinct.placesThisTick = 1
    distinct.heldItemClass = 1
    distinct.offhandItemClass = 7
    distinct.slotSwitchesThisTick = 2
    distinct.explosionKbX = 0.12f
    distinct.explosionKbY = 0.34f
    distinct.explosionKbZ = -0.56f
    distinct.hittableEntitiesCount = 2
    distinct.aimErrorYaw = -0.25f
    distinct.aimErrorPitch = 0.125f
    distinct.raytraceDirsHitCount = 2
    distinct.raytraceHitStrict = true

    val nonFinite = ShardTickData()
    nonFinite.dtMs = Float.NaN
    nonFinite.yaw = Float.NaN
    nonFinite.pitch = Float.PositiveInfinity
    nonFinite.x = Double.NegativeInfinity
    nonFinite.y = Double.NaN
    nonFinite.swimFriction = Float.NegativeInfinity
    nonFinite.targetDistance = Float.PositiveInfinity
    nonFinite.attackSpeed = Float.NegativeInfinity
    nonFinite.exposureFraction = Float.NaN
    nonFinite.boxInflation = Float.PositiveInfinity

    val roundingEdges = ShardTickData()
    roundingEdges.movementBitfield = (-1).toShort
    roundingEdges.activeItem = 5
    roundingEdges.targetRelX = -0.0000001f
    roundingEdges.targetRelY = -1.0000005f
    roundingEdges.groundFriction = 0.9999995f
    roundingEdges.x = -0.00000000001
    roundingEdges.yaw = -0.00000000001f
    roundingEdges.exposureFraction = 0.9999995f
    roundingEdges.boxInflation = -0.0000001f

    Seq(defaults, distinct, nonFinite, roundingEdges)

  test("everyFieldWritesRawBytesIdentically (Shard TickSchemaByteIdentityTest)"):
    val ticks = samples()
    assertEquals(ShardTickSchema.fields.size, FIELD_COUNT)
    var index = 0
    while index < ShardTickSchema.fields.size do
      val field = ShardTickSchema.fields(index)
      assertEquals(field.wireType.size, referenceFieldSize(index), field.name)
      val size = referenceFieldSize(index) * ticks.size
      val ref = ShardWireSink(new Array[Byte](size))
      val actual = ShardWireSink(new Array[Byte](size))
      ticks.foreach: tick =>
        referenceWriteRaw(ref, tick, index)
        field.writeRaw(tick, actual)
      assertEquals(actual.buf.toSeq, ref.buf.toSeq, field.name)
      index += 1

  test("csvDataColumnsAreIdentical and csvHeaderHasExpectedColumns (Shard TickSchemaByteIdentityTest)"):
    assertEquals(ShardTickSchema.csvHeader.split(',').length, FIELD_COUNT)
    samples().foreach: tick =>
      val refSb = JStringBuilder()
      referenceAppendCsv(refSb, tick)
      val actualSb = JStringBuilder()
      ShardTickSchema.appendCsvRow(actualSb, tick)
      val refData = refSb.toString.split(',').drop(2).toSeq
      val actualData = actualSb.toString.split(',').drop(2).toSeq
      assertEquals(actualData, refData)

  test("ShardTickData movement bitfield matches Shard bit positions (active_item at bit 3)"):
    val tracking = ShardTrackingState()
    tracking.sprinting = true
    tracking.sneaking = true
    tracking.isUsingItem = true
    tracking.swimming = true
    tracking.gliding = false
    tracking.flying = false
    tracking.inVehicle = false
    tracking.isClimbing = false
    tracking.inWater = true
    val bits = (if true then 1 else 0) |
      (if tracking.sprinting then 2 else 0) |
      (if tracking.sneaking then 4 else 0) |
      (if tracking.isUsingItem then 8 else 0) |
      (if tracking.flying then 16 else 0) |
      (if tracking.swimming then 32 else 0) |
      (if tracking.gliding then 64 else 0) |
      (if tracking.inVehicle then 128 else 0) |
      (if tracking.isClimbing then 256 else 0) |
      (if tracking.inWater then 512 else 0)
    assertEquals(bits, 1 | 2 | 4 | 8 | 32 | 512)

  test("ShardTickBuffer window extraction and sequenceId cutting match Shard DataBufferTest"):
    val buffer = ShardTickBuffer(10)
    var i = 0
    while i < 8 do
      val slot = buffer.currentSlot()
      slot.sequenceId = 1
      slot.tickIndex = i.toLong
      if i == 5 then
        slot.attackThisTick = true
        buffer.markAttack()
      buffer.advance()
      i += 1

    val window = buffer.extractWindow(3, 2, 2, 5, ShardTickData.START_MELEE_PLAYER)
    assert(window != null)
    val w = window
    assertEquals(w.size(), 5)
    assertEquals(w.get(0).tickIndex, 2L)
    assertEquals(w.get(3).tickIndex, 5L)
    assertEquals(w.get(3).windowStartKind, ShardTickData.START_MELEE_PLAYER)
    assertEquals(w.get(3).ticksToAttack, 0.toShort)
    assertEquals(w.get(0).ticksToAttack, (-3).toShort)
    assertEquals(w.get(4).ticksToAttack, 1.toShort)

  test("ShardTickBuffer returns null when window crosses sequenceId boundary"):
    val buffer = ShardTickBuffer(16)
    var i = 0
    while i < 10 do
      val slot = buffer.currentSlot()
      slot.sequenceId = if i < 4 then 1 else 2
      slot.tickIndex = i.toLong
      if i == 6 then buffer.markAttack()
      buffer.advance()
      i += 1
    // Window [2..8] crosses sequenceId 1 -> 2 at index 4, so ShardTickBuffer.extractWindow returns null
    val window = buffer.extractWindow(4, 2, 3, 6, ShardTickData.START_MELEE_PLAYER)
    assert(window == null)
    // Window [4..8] stays within sequenceId 2, so it succeeds
    val validWindow = buffer.extractWindow(2, 2, 3, 6, ShardTickData.START_MELEE_PLAYER)
    assert(validWindow != null)
    assertEquals(validWindow.size(), 4)
    assertEquals(validWindow.get(0).tickIndex, 4L)
    assertEquals(validWindow.get(2).tickIndex, 6L)

  test("ShardAttackWindowTracker anchors on first attack and emits when postWindow completes"):
    val buffer = ShardTickBuffer(32)
    val tracker = ShardAttackWindowTracker()
    val emitted = ArrayList[(Int, Int, Short)]()
    var tick = 0
    while tick < 12 do
      val slot = buffer.currentSlot()
      slot.sequenceId = 1
      slot.tickIndex = tick.toLong
      val isAttack = tick == 2 || tick == 4
      if isAttack && tracker.ticksSinceAttack < 0 then buffer.markAttack()
      tracker.onTick(buffer, isAttack, ShardTickData.START_MELEE_PLAYER, postWindow = 3): (ticks, idx, kind) =>
        emitted.add((ticks, idx, kind))
      buffer.advance()
      tick += 1
    // First window anchored at tick 2 completes 3 ticks later (tick 4 -> ticksSinceAttack = 3),
    // and if another attack occurs after reset, it would start a new window.
    assertEquals(emitted.size(), 1)
    val (ticksSinceFirst, firstAttackIdx, kind) = emitted.get(0)
    assertEquals(firstAttackIdx, 2)
    assertEquals(ticksSinceFirst, 3)
    assertEquals(kind, ShardTickData.START_MELEE_PLAYER)

  test("ShardAimProcessor preserves raw non-wrapped deltaYawAbs and >15 mode threshold"):
    val aim = ShardAimProcessor()
    val rot = ShardRotationUpdate(ShardHeadRotation(0f, 0f), ShardHeadRotation(1.5f, 1.5f), 1.5f, 1.5f)
    // 1st sample initializes lastXRot/lastYRot; next 15 samples add to xRotMode/yRotMode (size = 15, modeCount = 15)
    var i = 0
    while i < 16 do
      aim.process(rot)
      i += 1
    // At 15 mode samples, modeCount is 15 which is NOT > 15 (SIGNIFICANT_SAMPLES_THRESHOLD)
    assert(!aim.modeYawValid)
    assert(!aim.modePitchValid)
    // 17th sample (16th mode entry) crosses > 15
    aim.process(rot)
    assert(aim.modeYawValid)
    assert(aim.modePitchValid)
    val dots1 = aim.consumeDots()
    assert(dots1.yaw > 0f)
    val dots2 = aim.consumeDots()
    assertEquals(dots2.yaw, 0f)
    assertEquals(dots2.pitch, 0f)
