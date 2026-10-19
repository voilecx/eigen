/*
 * This file is part of Eigen.
 * Copyright (C) 2026 Eigen contributors
 *
 * Portions of this file are derived from the following projects, also licensed under
 * the GNU General Public License version 3:
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

package eigen.core.sim

import eigen.core.entity.TrackedEntity
import eigen.core.history.{Window, WindowKind}
import eigen.core.geom.Box
import eigen.core.schema.Schema

/** One stage of the per-tick pipeline. Systems are stateless; all state lives in components. */
trait TickSystem:
  def run(s: PlayerSim): Unit

/**
 * The fixed order systems run in at the end of every client tick. Order matters: each system
 * reads what the previous ones derived, and [[RecordSystem]] snapshots the result.
 */
object Schedule:
  private val systems: Array[TickSystem] = Array(
    EntitySystem,
    RotationSystem,
    InputSystem,
    TargetSystem,
    GateSystem,
    RecordSystem,
    WindowSystem,
    CleanupSystem,
  )

  def run(s: PlayerSim): Unit =
    var i = 0
    while i < systems.length do
      systems(i).run(s)
      i += 1

/** Advances every tracked entity's interpolation by one client tick. */
object EntitySystem extends TickSystem:
  def run(s: PlayerSim): Unit = s.entities.foreachValue(_.tick())

/** Turns the tick's rotation into look vectors and sensitivity-normalised mouse counts. */
object RotationSystem extends TickSystem:
  def run(s: PlayerSim): Unit =
    val r = s.rotation
    val doubleIndex = s.profile.doubleIndexTrig
    r.look.fromRotation(r.yaw, r.pitch, doubleIndex)
    r.lastYawLook.fromRotation(r.lastYaw, r.pitch, doubleIndex)
    r.lastLook.fromRotation(r.lastYaw, r.lastPitch, doubleIndex)

    r.dotsYaw = 0; r.dotsPitch = 0; r.residYaw = 0; r.residPitch = 0
    if r.forced || r.discontinuity then return
    if r.tickYaw != 0 then
      val ulp = math.ulp(math.max(math.abs(r.yaw), math.abs(r.lastYaw))).toDouble
      r.yawAxis.observe(r.tickYaw, ulp)
    if r.tickPitch != 0 then r.pitchAxis.observe(r.tickPitch, math.ulp(90f).toDouble)
    // An axis that did not move contributes exactly zero counts, calibrated or not.
    if r.yawAxis.converged then
      val dy = r.yawAxis.dots(r.tickYaw)
      r.dotsYaw = dy.toFloat
      r.residYaw = (dy - math.rint(dy)).toFloat
    if r.pitchAxis.converged then
      val dp = r.pitchAxis.dots(r.tickPitch)
      r.dotsPitch = dp.toFloat
      r.residPitch = (dp - math.rint(dp)).toFloat

  /** True when this tick's mouse counts are trustworthy on both axes. */
  def calibrated(r: Rotation): Boolean =
    (r.tickYaw == 0 || r.yawAxis.converged) && (r.tickPitch == 0 || r.pitchAxis.converged) &&
      (r.yawAxis.converged || r.pitchAxis.converged)

/**
 * Body motion bookkeeping and, for clients older than 1.21.2, movement keys inferred from the
 * horizontal motion relative to the camera.
 */
object InputSystem extends TickSystem:
  private inline val KeyThreshold = 0.02
  private inline val JumpRise = 0.1

  def run(s: PlayerSim): Unit =
    val b = s.body
    val dy = b.y - b.lastY
    if b.onGround then b.fallDistance = 0
    else if dy < 0 then b.fallDistance -= dy

    if !s.profile.realInput then
      val k = s.keys
      val mx = b.x - b.lastX
      val mz = b.z - b.lastZ
      val yawRad = s.rotation.yaw * eigen.core.geom.Angles.DegToRad
      val sin = math.sin(yawRad)
      val cos = math.cos(yawRad)
      // Forward is (-sin, cos); right is (-cos, -sin) in Minecraft's x-east, z-south frame.
      val ahead = -sin * mx + cos * mz
      val side = -cos * mx - sin * mz
      k.forward = ahead > KeyThreshold; k.backward = ahead < -KeyThreshold
      k.right = side > KeyThreshold; k.left = side < -KeyThreshold
      k.jump = b.wasOnGround && !b.onGround && dy > JumpRise
      k.sprint = b.sprinting
      k.shift = b.sneaking

/** Picks the melee target and derives its geometry in the player's view frame. */
object TargetSystem extends TickSystem:
  private inline val StandingEye = 1.62
  private inline val LowEye = 0.4

  def run(s: PlayerSim): Unit =
    val c = s.combat
    val b = s.body
    val r = s.rotation
    val cfg = s.config
    val eyeHeight = (if b.swimming || b.gliding then LowEye else if b.sneaking then s.profile.sneakEyeHeight else StandingEye) * b.scale
    val ex = b.x
    val ey = b.y + eyeHeight
    val ez = b.z
    val look = r.look
    val union = s.boxA

    // Combat is defined by hits: the attacked entity is the target while the last hit is recent.
    // Nothing before the first hit of a fight is combat, so pre-hit aim is not modelled.
    val attacked = s.entity(s.clicks.attackTarget)
    val recent = s.tick - s.clicks.lastAttackTick <= cfg.combatTicks
    val target: TrackedEntity | Null =
      if recent && attacked != null && !attacked.dead && s.targetable(attacked.kind) then attacked else null
    c.target = target
    c.gate = target != null

    if target == null then
      clear(c)
      return

    // Best estimate of what the client renders: the centre of the possible area, true size.
    target.possibleHitbox(union)
    val tight = target.possibleLocations(s.boxB)
    val fx = tight.centerX
    val fy = tight.minY + (tight.maxY - tight.minY) * 0.5
    val fz = tight.centerZ
    tight.set(fx, fy, fz, fx, fy, fz).growByDimensions(target.width, target.height)
    c.inflation = ((union.sizeX - tight.sizeX) + (union.sizeY - tight.sizeY) + (union.sizeZ - tight.sizeZ)).toFloat

    Geometry.aimError(ex, ey, ez, r.yaw, r.pitch, tight, s.pair)
    c.aimYaw = s.pair(0).toFloat
    c.aimPitch = s.pair(1).toFloat
    c.aimAngle = Geometry.minAngleToBox(ex, ey, ez, look, union).toFloat
    c.distance = union.distanceTo(ex, ey, ez).toFloat

    // View frame: forward = look, right = horizontal right of yaw, up = right × forward.
    val dx = tight.centerX - ex
    val dy = tight.centerY - ey
    val dz = tight.centerZ - ez
    val yawRad = r.yaw * eigen.core.geom.Angles.DegToRad
    val rx = -math.cos(yawRad)
    val rz = -math.sin(yawRad)
    val ux = -rz * look.y
    val uy = rz * look.x - rx * look.z
    val uz = rx * look.y
    c.relFwd = (dx * look.x + dy * look.y + dz * look.z).toFloat
    c.relSide = (dx * rx + dz * rz).toFloat
    c.relUp = (dx * ux + dy * uy + dz * uz).toFloat

    if s.clicks.attack && s.clicks.attackTarget == target.id then hitTest(s, ex, ey, ez, tight)
    else
      c.hit = false; c.hitX = 0; c.hitY = 0; c.hitZ = 0; c.miss = 0; c.crit = false

  /** Ray test of the attack against the target, trying every look the client may have used. */
  private def hitTest(s: PlayerSim, ex: Double, ey: Double, ez: Double, box: Box): Unit =
    val c = s.combat
    val r = s.rotation
    val reach = s.body.interactionRange
    val point = s.pair
    var dist = Geometry.rayHit(ex, ey, ez, r.look, reach, box, point)
    if dist < 0 && s.profile.lookCandidates >= 2 then dist = Geometry.rayHit(ex, ey, ez, r.lastYawLook, reach, box, point)
    if dist < 0 && s.profile.lookCandidates >= 3 then dist = Geometry.rayHit(ex, ey, ez, r.lastLook, reach, box, point)
    c.hit = dist >= 0
    if c.hit then
      c.hitX = ((point(0) - box.centerX) / (box.sizeX * 0.5)).toFloat
      c.hitY = ((point(1) - box.centerY) / (box.sizeY * 0.5)).toFloat
      c.hitZ = ((point(2) - box.centerZ) / (box.sizeZ * 0.5)).toFloat
      c.miss = 0
    else
      c.hitX = 0; c.hitY = 0; c.hitZ = 0
      c.miss = Geometry.missDistance(ex, ey, ez, r.look, reach, box).toFloat
    val b = s.body
    c.crit = s.clicks.strengthAtAttack > 0.9f && b.fallDistance > 0 && !b.onGround &&
      !s.clicks.sprintAtAttack && !b.inVehicle && !b.gliding && !b.swimming

  private def clear(c: Combat): Unit =
    c.aimYaw = 0; c.aimPitch = 0; c.aimAngle = 0; c.distance = 0
    c.relFwd = 0; c.relSide = 0; c.relUp = 0
    c.hit = false; c.hitX = 0; c.hitY = 0; c.hitZ = 0; c.miss = 0; c.crit = false
    c.inflation = 0

/** Tracks engagements: entering combat after at least a buffer's worth of calm starts one. */
object GateSystem extends TickSystem:
  def run(s: PlayerSim): Unit =
    val c = s.combat
    c.engagedThisTick = false
    if c.gate then
      if s.tick - c.lastCombatTick > s.config.bufferTicks then
        c.engagement += 1
        c.engagedThisTick = true
      c.lastCombatTick = s.tick

/** Derives masks and meta, then writes the tick's row into the history. */
object RecordSystem extends TickSystem:
  import Schema.*

  def run(s: PlayerSim): Unit =
    val h = s.history
    val r = s.rotation
    val k = s.keys
    val b = s.body
    val cl = s.clicks
    val c = s.combat
    val cond = s.conditions

    // Idle: no mouse, keys or clicks for a while.
    if r.rotated || k.any || cl.swing || cl.use || cl.attack then cond.idleRun = 0 else cond.idleRun += 1

    val reasons = reasonsOf(s)
    val activity = activityOf(s)

    val attackOnTarget = cl.attack && c.target != null && cl.attackTarget == c.target.nn.id

    h(DotsYaw) = r.dotsYaw
    h(DotsPitch) = r.dotsPitch
    h(ResidYaw) = r.residYaw
    h(ResidPitch) = r.residPitch

    h(Strafe) = k.strafe.toFloat
    h(Forward) = k.ahead.toFloat
    h(Jump) = k.jump
    h(Sprint) = b.sprinting
    h(Sneak) = b.sneaking
    h(Swing) = cl.swing
    h(Use) = cl.use

    h(AimYaw) = c.aimYaw
    h(AimPitch) = c.aimPitch
    h(AimAngle) = c.aimAngle
    h(Distance) = c.distance
    h(RelFwd) = c.relFwd
    h(RelSide) = c.relSide
    h(RelUp) = c.relUp
    h(Attack) = attackOnTarget
    h(Hit) = attackOnTarget && c.hit
    h(HitX) = c.hitX
    h(HitY) = c.hitY
    h(HitZ) = c.hitZ
    h(Miss) = c.miss
    h(Cooldown) = if cl.attack then cl.strengthAtAttack else cl.strength(cl.strengthTicks)
    h(Crit) = attackOnTarget && c.crit
    h(SprintHit) = attackOnTarget && cl.sprintAtAttack
    h(SameTarget) = attackOnTarget && cl.prevAttackTarget == cl.attackTarget

    h(MaskRot) = reasons.disjointFrom(Reason.MasksRot)
    h(MaskInp) = reasons.disjointFrom(Reason.MasksInp)
    h(MaskTgt) = reasons.disjointFrom(Reason.MasksTgt)

    h(Reasons) = reasons.toFloat
    h(Gate) = c.gate
    h(Activity) = activity.toFloat
    h(Inflation) = c.inflation
    h(TargetKind) = (if c.target == null then 0 else c.target.nn.kind.code).toFloat
    h(ItemKind) = s.inventory.held.toFloat
    h(PingBucket) = s.pingBucket.toFloat

    h.commit(s.tick, s.epoch)

  /** Why this tick's streams are masked, as a set of `Reason` bits. */
  private def reasonsOf(s: PlayerSim): Reason.Reason =
    val r = s.rotation
    val b = s.body
    val cond = s.conditions
    val c = s.combat
    Reason.Teleport.when(s.tick <= cond.teleportUntil)
      | Reason.ServerRotation.when(r.forced)
      | Reason.Uncalibrated.when(!RotationSystem.calibrated(r))
      | Reason.Gui.when(cond.guiOpen)
      | Reason.Vehicle.when(b.inVehicle)
      | Reason.Gliding.when(b.gliding)
      | Reason.Riptide.when(b.riptide)
      | Reason.Knockback.when(s.tick <= cond.knockbackUntil)
      | Reason.Idle.when(cond.idleRun >= s.config.idleTicks)
      | Reason.NoTarget.when(c.target == null)
      | Reason.Uncertain.when(c.target != null && c.inflation > s.config.maxInflation)
      | Reason.Water.when(b.swimming || b.inWater)
      | Reason.LegacyInput.when(!s.profile.realInput)
      | Reason.Discontinuity.when(r.discontinuity)
      | Reason.NoMoveTick.when(!s.flyingThisTick)
      | Reason.Attack.when(s.clicks.attack)

  /** What the player was doing this tick, as a set of `ActivityBit` bits. */
  private def activityOf(s: PlayerSim): ActivityBit.Activity =
    val cl = s.clicks
    ActivityBit.Moving.when(s.keys.any)
      | ActivityBit.Mining.when(s.tick - cl.lastDigTick <= RecentTicks)
      | ActivityBit.Building.when(s.tick - cl.lastPlaceTick <= RecentTicks)
      | ActivityBit.Rotating.when(s.rotation.rotated)

  private inline val RecentTicks = 10

/** A window waiting for its post-anchor rows. */
final class PendingCut:
  var active = false
  var anchorSeq = 0L
  var anchorTick = 0
  var engagement = 0

/** Cuts attack, engage and calm windows out of the history as rows become final. */
object WindowSystem extends TickSystem:
  def run(s: PlayerSim): Unit =
    val h = s.history
    val seq = h.nextSeq - 1 // the row just recorded
    val cfg = s.config
    val c = s.combat

    if s.windowsWanted then
      val attacked = h(Schema.Attack, seq) != 0f
      if attacked && !s.attackCut.active && s.tick - s.attackCut.anchorTick >= cfg.attackMinGap then
        arm(s.attackCut, seq, s.tick, c.engagement)
      if c.engagedThisTick && !s.engageCut.active then arm(s.engageCut, seq, s.tick, c.engagement)
      close(s, s.attackCut, WindowKind.Attack, cfg.attackPre, cfg.attackPost)
      close(s, s.engageCut, WindowKind.Engage, cfg.engagePre, cfg.engagePost)

    // Calm classification runs `bufferTicks` behind, when combat on either side is known.
    val delayed = seq - cfg.bufferTicks
    if delayed >= 0 && h.holds(delayed) then
      val tickThen = h.tickAt(delayed)
      val calmRow = h(Schema.Gate, delayed) == 0f && tickThen - c.lastCombatTick > cfg.bufferTicks
      if !s.windowsWanted then s.calm.flush()
      else if calmRow then
        val done = s.calm.append(h, delayed, c.engagement)
        if done != null then s.emitWindow(done)
      else flushCalm(s)

  def flushCalm(s: PlayerSim): Unit =
    val done = s.calm.flush()
    if done != null && s.windowsWanted then s.emitWindow(done)

  private def arm(cut: PendingCut, seq: Long, tick: Int, engagement: Int): Unit =
    cut.active = true
    cut.anchorSeq = seq
    cut.anchorTick = tick
    cut.engagement = engagement

  private def close(s: PlayerSim, cut: PendingCut, kind: WindowKind, pre: Int, post: Int): Unit =
    if !cut.active || s.history.nextSeq < cut.anchorSeq + post then return
    cut.active = false
    val h = s.history
    val epoch = h.epochAt(cut.anchorSeq)
    var from = math.max(h.oldestSeq, cut.anchorSeq - pre)
    while from < cut.anchorSeq && h.epochAt(from) != epoch do from += 1
    val rows = (cut.anchorSeq + post - from).toInt
    if h.contiguous(from, rows) then
      s.emitWindow(Window.cut(h, kind, Schema.Combat, from, rows, (cut.anchorSeq - from).toInt, cut.engagement))

/** Clears per-tick event flags and advances tick-based timers. */
object CleanupSystem extends TickSystem:
  def run(s: PlayerSim): Unit =
    val r = s.rotation
    r.tickYaw = 0; r.tickPitch = 0
    r.rotated = false; r.forced = false; r.discontinuity = false
    val cl = s.clicks
    // A swing that hit nothing resets the attack strength like a hit does.
    if cl.swing && !cl.attack then cl.strengthTicks = 0 else cl.strengthTicks += 1
    cl.swing = false; cl.use = false; cl.attack = false
    val b = s.body
    b.lastX = b.x; b.lastY = b.y; b.lastZ = b.z
    b.wasOnGround = b.onGround
