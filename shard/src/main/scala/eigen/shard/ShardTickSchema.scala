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

import java.lang.invoke.{MethodHandles, VarHandle}
import java.nio.ByteOrder

final class ShardWireSink(val buf: Array[Byte]):
  import ShardWireSink.*
  private var pos = 0

  def position: Int = pos
  def remaining: Int = buf.length - pos

  def put(v: Byte): Unit =
    buf(pos) = v
    pos += 1

  def putShort(v: Short): Unit =
    Short16.set(buf, pos, v)
    pos += 2

  def putInt(v: Int): Unit =
    Int32.set(buf, pos, v)
    pos += 4

  def putFloat(v: Float): Unit =
    Float32.set(buf, pos, v)
    pos += 4

  def putDouble(v: Double): Unit =
    Double64.set(buf, pos, v)
    pos += 8

object ShardWireSink:
  private val Short16: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Short]], ByteOrder.LITTLE_ENDIAN)
  private val Int32: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Int]], ByteOrder.LITTLE_ENDIAN)
  private val Float32: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Float]], ByteOrder.LITTLE_ENDIAN)
  private val Double64: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Double]], ByteOrder.LITTLE_ENDIAN)

object ShardTickSchema:

  enum WireType(val size: Int):
    case BOOL extends WireType(1)
    case I8 extends WireType(1)
    case U8 extends WireType(1)
    case I16 extends WireType(2)
    case I32 extends WireType(4)
    case F32 extends WireType(4)
    case F64 extends WireType(8)

  final class Field(
      val name: String,
      val writeCsv: (ShardTickData, Appendable) => Unit,
      val rawFloat: ShardTickData => Float,
      val wireType: WireType,
      val writeRaw: (ShardTickData, ShardWireSink) => Unit,
  )

  private inline val U8_MAX = 0xff
  private inline val FLOAT_SCALE = 1_000_000L
  private inline val DOUBLE_SCALE = 10_000_000_000L
  private val LOG = java.util.logging.Logger.getLogger("Shard")
  private val clamped = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()

  val fields: IndexedSeq[Field] = IndexedSeq(
    i32("sequence_id")(_.sequenceId),
    i32("tick_index")(_.tickIndex.toInt),
    f32("dt_ms")(_.dtMs),
    i32("target_entity_id")(_.targetEntityId),
    f32hp("yaw")(_.yaw),
    f32hp("pitch")(_.pitch),
    f64("x")(_.x),
    f64("y")(_.y),
    f64("z")(_.z),
    bit("on_ground", 0),
    bit("sprinting", 1),
    bit("sneaking", 2),
    i16("active_item")(_.activeItem),
    bit("flying", 4),
    bit("swimming", 5),
    bit("gliding", 6),
    bit("in_vehicle", 7),
    bit("is_climbing", 8),
    bit("in_water", 9),
    i8("pose")(_.pose),
    bool("input_forward")(_.inputForward),
    bool("input_backward")(_.inputBackward),
    bool("input_left")(_.inputLeft),
    bool("input_right")(_.inputRight),
    bool("input_jump")(_.inputJump),
    bool("input_shift")(_.inputShift),
    bool("input_sprint")(_.inputSprint),
    i8("game_mode")(_.gameMode),
    u8("food_level")(_.foodLevel),
    f32("ground_friction")(_.groundFriction),
    f32("fall_distance")(_.fallDistance),
    f32("movement_speed")(_.movementSpeed),
    i32("jump_amplifier")(_.jumpAmplifier),
    bool("slow_falling")(_.slowFalling),
    i32("player_ping")(_.playerPing),
    i32("speed_amplifier")(_.speedAmplifier),
    i32("slowness_amplifier")(_.slownessAmplifier),
    i32("haste_amplifier")(_.hasteAmplifier),
    i32("mining_fatigue_amplifier")(_.miningFatigueAmplifier),
    f32("entity_interaction_range")(_.entityInteractionRange),
    f32("scale")(_.scale),
    f32("gravity")(_.gravity),
    f32("jump_strength")(_.jumpStrength),
    f32("step_height")(_.stepHeight),
    bool("target_valid")(_.targetValid),
    f32("target_distance")(_.targetDistance),
    f32("target_aim_angle")(_.targetAimAngle),
    f32("target_rel_x")(_.targetRelX),
    f32("target_rel_y")(_.targetRelY),
    f32("target_rel_z")(_.targetRelZ),
    i16("ticks_to_attack")(_.ticksToAttack),
    bool("attack_this_tick")(_.attackThisTick),
    f32("aim_error")(_.aimError),
    bool("same_target_as_prev_attack")(_.sameTargetAsPrevAttack),
    u8("targets_in_range_count")(_.targetsInRangeCount),
    bool("is_critical")(_.isCritical),
    bool("raytrace_hit")(_.raytraceHit),
    bool("attack_ray_occluded")(_.attackRayOccluded),
    f32("raytrace_miss_distance")(_.raytraceMissDistance),
    bool("input_valid")(_.inputValid),
    f32("hit_point_rel_x")(_.hitPointRelX),
    f32("hit_point_rel_y")(_.hitPointRelY),
    f32("hit_point_rel_z")(_.hitPointRelZ),
    u8("ticks_since_attack")(_.ticksSinceAttack),
    bool("swung_this_tick")(_.swungThisTick),
    f32("attack_cooldown_progress")(_.attackCooldownProgress),
    f32("attack_speed")(_.attackSpeed),
    bool("is_sprinting_on_attack")(_.isSprintingOnAttack),
    bool("received_velocity_this_tick")(_.receivedVelocityThisTick),
    f32("velocity_x")(_.velocityX),
    f32("velocity_y")(_.velocityY),
    f32("velocity_z")(_.velocityZ),
    u8("ticks_since_velocity")(_.ticksSinceVelocity),
    bool("received_explosion_this_tick")(_.receivedExplosionThisTick),
    u8("ticks_since_explosion")(_.ticksSinceExplosion),
    u8("ticks_since_use_item")(_.ticksSinceUseItem),
    bool("rotation_present")(_.rotationPresent),
    f32("mode_yaw")(_.modeYaw),
    f32("mode_pitch")(_.modePitch),
    bool("mode_yaw_valid")(_.modeYawValid),
    bool("mode_pitch_valid")(_.modePitchValid),
    f32("delta_dots_yaw")(_.deltaDotsYaw),
    f32("delta_dots_pitch")(_.deltaDotsPitch),
    u8("active_firework_count")(_.activeFireworkCount),
    f32("stuck_mult_x")(_.stuckMultX),
    f32("stuck_mult_y")(_.stuckMultY),
    f32("stuck_mult_z")(_.stuckMultZ),
    bool("on_slime")(_.onSlime),
    bool("on_honey")(_.onHoney),
    bool("on_soul_sand")(_.onSoulSand),
    bool("on_mud")(_.onMud),
    i32("levitation_amplifier")(_.levitationAmplifier),
    f32("swim_friction")(_.swimFriction),
    bool("riptide_active")(_.riptideActive),
    bool("teleport_tick")(_.teleportTick),
    f32("exposure_fraction")(_.exposureFraction),
    f32("box_inflation")(_.boxInflation),
    f32("health")(_.health),
    f32("damage_taken_this_tick")(_.damageTakenThisTick),
    u8("ticks_since_damage")(_.ticksSinceDamage),
    i16("window_start")(_.windowStartKind),
    i16("target_type")(_.targetType),
    i16("crystal_spawn_to_attack")(_.crystalSpawnToAttack),
    bool("use_offhand")(_.useOffhand),
    u8("nearby_crystal_count")(_.nearbyCrystalCount),
    i16("anchor_charge")(_.anchorCharge),
    i16("anchor_use_interval")(_.anchorUseInterval),
    i16("place_face")(_.placeFace),
    i16("place_block_class")(_.placeBlockClass),
    i32("place_block_x")(_.placeBlockX),
    i32("place_block_y")(_.placeBlockY),
    i32("place_block_z")(_.placeBlockZ),
    f32("place_cursor_x")(_.placeCursorX),
    f32("place_cursor_y")(_.placeCursorY),
    f32("place_cursor_z")(_.placeCursorZ),
    bool("place_inside_block")(_.placeInsideBlock),
    i16("places_this_tick")(_.placesThisTick),
    i16("held_item_class")(_.heldItemClass),
    i16("offhand_item_class")(_.offhandItemClass),
    i16("slot_switches_this_tick")(_.slotSwitchesThisTick),
    f32("explosion_kb_x")(_.explosionKbX),
    f32("explosion_kb_y")(_.explosionKbY),
    f32("explosion_kb_z")(_.explosionKbZ),
    u8("hittable_entities_count")(_.hittableEntitiesCount),
    f32("aim_error_yaw")(_.aimErrorYaw),
    f32("aim_error_pitch")(_.aimErrorPitch),
    i16("raytrace_dirs_hit_count")(_.raytraceDirsHitCount),
    bool("raytrace_hit_strict")(_.raytraceHitStrict),
  )

  val csvHeader: String = fields.map(_.name).mkString(",")

  val rowSize: Int = fields.map(_.wireType.size).sum

  val fieldNames: IndexedSeq[String] = fields.map(_.name)

  val fieldsByName: Map[String, Field] = fields.map(f => f.name -> f).toMap

  def appendCsvRow(out: Appendable, tick: ShardTickData): Unit =
    var i = 0
    val len = fields.length
    while i < len do
      if i > 0 then out.append(',')
      fields(i).writeCsv(tick, out)
      i += 1

  private def i32(name: String)(get: ShardTickData => Int): Field =
    Field(
      name,
      (t, o) => { val _ = o.append(get(t).toString) },
      t => get(t).toFloat,
      WireType.I32,
      (t, b) => b.putInt(get(t)),
    )

  private def i8(name: String)(get: ShardTickData => Int): Field =
    narrow(name, get, WireType.I8, Byte.MinValue.toInt, Byte.MaxValue.toInt)

  private def u8(name: String)(get: ShardTickData => Int): Field =
    narrow(name, get, WireType.U8, 0, U8_MAX)

  private def narrow(name: String, get: ShardTickData => Int, tpe: WireType, min: Int, max: Int): Field =
    Field(
      name,
      (t, o) => { val _ = o.append(get(t).toString) },
      t => get(t).toFloat,
      tpe,
      (t, b) =>
        val v = get(t)
        if (v < min || v > max) && clamped.add(name) then
          LOG.severe(s"[AI] $name = $v does not fit $tpe on the wire; clamped to $min..$max")
        val clampedVal = if v < min then min else if v > max then max else v
        b.put(clampedVal.toByte)
      ,
    )

  private def i16(name: String)(get: ShardTickData => Short): Field =
    Field(
      name,
      (t, o) => { val _ = o.append(get(t).toString) },
      t => get(t).toFloat,
      WireType.I16,
      (t, b) => b.putShort(get(t)),
    )

  private def f32(name: String)(get: ShardTickData => Float): Field =
    Field(
      name,
      (t, o) => appendFloat(o, get(t)),
      t => get(t),
      WireType.F32,
      (t, b) => b.putFloat(get(t)),
    )

  private def f32hp(name: String)(get: ShardTickData => Float): Field =
    Field(
      name,
      (t, o) => appendDouble(o, get(t).toDouble),
      t => get(t),
      WireType.F32,
      (t, b) => b.putFloat(get(t)),
    )

  private def f64(name: String)(get: ShardTickData => Double): Field =
    Field(
      name,
      (t, o) => appendDouble(o, get(t)),
      t => get(t).toFloat,
      WireType.F64,
      (t, b) => b.putDouble(get(t)),
    )

  private def bool(name: String)(get: ShardTickData => Boolean): Field =
    Field(
      name,
      (t, o) => { val _ = o.append(if get(t) then '1' else '0') },
      t => if get(t) then 1f else 0f,
      WireType.BOOL,
      (t, b) => b.put(if get(t) then 1.toByte else 0.toByte),
    )

  private def bit(name: String, index: Int): Field =
    Field(
      name,
      (t, o) => { val _ = o.append(if ((t.movementBitfield.toInt >> index) & 1) == 1 then '1' else '0') },
      t => if ((t.movementBitfield.toInt >> index) & 1) == 1 then 1f else 0f,
      WireType.BOOL,
      (t, b) => b.put(if ((t.movementBitfield.toInt >> index) & 1) == 1 then 1.toByte else 0.toByte),
    )

  private def appendFloat(out: Appendable, value: Float): Unit =
    if !java.lang.Float.isFinite(value) then
      out.append("0.000000")
      return
    var v = value.toDouble
    val negative = v < 0.0
    if negative then v = -v
    val scaled = (v * FLOAT_SCALE + 0.5).toLong
    val intPart = scaled / FLOAT_SCALE
    val fracPart = (scaled % FLOAT_SCALE).toInt
    if negative && scaled > 0 then out.append('-')
    out.append(intPart.toString)
    out.append('.')
    val fracText = fracPart.toString
    var pad = 6 - fracText.length
    while pad > 0 do
      out.append('0')
      pad -= 1
    out.append(fracText)

  private def appendDouble(out: Appendable, value: Double): Unit =
    if !java.lang.Double.isFinite(value) then
      out.append("0.0000000000")
      return
    var v = value
    val negative = v < 0.0
    if negative then v = -v
    val scaled = (v * DOUBLE_SCALE + 0.5).toLong
    val intPart = scaled / DOUBLE_SCALE
    val fracPart = scaled % DOUBLE_SCALE
    if negative && scaled > 0 then out.append('-')
    out.append(intPart.toString)
    out.append('.')
    val fracText = fracPart.toString
    var pad = 10 - fracText.length
    while pad > 0 do
      out.append('0')
      pad -= 1
    out.append(fracText)
