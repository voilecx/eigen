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

package eigen.core.entity

import eigen.core.geom.Box

/** Coarse entity class, as reported in the `target_kind` meta column (0 = none). */
enum EntityKind(val code: Int):
  case Player extends EntityKind(1)
  case Living extends EntityKind(2)
  case Crystal extends EntityKind(3)
  case Other extends EntityKind(4)

/**
 * An entity as one particular client sees it: the server's latest position plus the window of
 * positions the client may still be interpolating through.
 *
 * Moves are applied in two transaction-bracketed steps, exactly when the client acknowledges
 * them (see [[eigen.core.latency.TransactionClock]]): [[onFirstTransaction]] when the move may
 * have been received, [[onSecondTransaction]] when it certainly was. Three interpolation slots
 * rotate so that steady movement allocates nothing.
 */
final class TrackedEntity(val id: Int, val kind: EntityKind, val steps: Int, profile: ClientProfile):
  import TrackedEntity.*

  /** Hitbox size as the client sees it; kept current by the platform adapter. */
  var width: Float = 0.6f
  var height: Float = 1.8f

  var dead = false

  /** Last transaction a move of this entity was bracketed with; used to force a fresh one. */
  var lastTransactionHung = 0

  // Latest server position, in the protocol's 1/4096-block fixed point.
  private var px = 0L
  private var py = 0L
  private var pz = 0L

  private var current: Interpolation | Null = null
  private var previous: Interpolation | Null = null
  private var spare: Interpolation | Null = null

  // Per-axis speed estimate (blocks per server update) bounding how far back a start can lie.
  private val speed = new Array[Double](3)
  private var hasLastTarget = false
  private var lastTargetX = 0.0
  private var lastTargetY = 0.0
  private var lastTargetZ = 0.0
  private var jump: Array[Double] | Null = null

  private val scratch = Box()

  def x: Double = px / Scale
  def y: Double = py / Scale
  def z: Double = pz / Scale

  def spawnAt(x: Double, y: Double, z: Double): Unit =
    if profile.coarsePositions then
      px = pack(math.floor(x * 32) / 32); py = pack(math.floor(y * 32) / 32); pz = pack(math.floor(z * 32) / 32)
    else
      px = pack(x); py = pack(y); pz = pack(z)

  /** The move may have reached the client: widen the possible area to cover it. */
  def onFirstTransaction(relative: Boolean, hasPos: Boolean, dx: Double, dy: Double, dz: Double): Unit =
    val preX = x
    val preY = y
    val preZ = z
    val old = current
    val startBox = if old != null then old.possibleLocations(scratch) else scratch.setPoint(x, y, z)

    if hasPos then
      if relative then
        if profile.floorPacking then
          if dx != 0 then px = math.floor(px + dx * Scale).toLong
          if dy != 0 then py = math.floor(py + dy * Scale).toLong
          if dz != 0 then pz = math.floor(pz + dz * Scale).toLong
        else
          px += math.round(dx * Scale); py += math.round(dy * Scale); pz += math.round(dz * Scale)
      else spawnAt(dx, dy, dz)
      updateSpeed(relative, preX, preY, preZ)

    // Retire the slot of the move before `old`; it can no longer matter.
    val recycled = previous
    previous = old
    val next = if spare != null then spare.nn else Interpolation()
    spare = recycled
    current =
      if !hasPos && profile.rotationFreezesInterpolation then next.hold(startBox)
      else
        next.reset(startBox, x, y, z, steps, profile.skipsIdleTicks)
        clampToSpeed(next)
        next

  /** The move certainly reached the client: the move before it has fully settled. */
  def onSecondTransaction(): Unit =
    if previous != null then
      if spare == null then spare = previous
      previous = null

  /** One client tick of interpolation. */
  def tick(): Unit =
    val cur = current
    if cur == null then return
    cur.tick(lowAdvances = previous == null)
    val prev = previous
    if prev != null then
      prev.tick(lowAdvances = true)
      cur.start.include(prev.possibleLocations(scratch))
      clampToSpeed(cur)

  /** Union of every hitbox the client may currently see, written into `out`. */
  def possibleHitbox(out: Box): Box =
    possibleLocations(out).growByDimensions(width, height)
    if profile.coarsePositions then out.expand(CoarseJitter, CoarseJitterY, CoarseJitter)
    out

  /** Union of the feet positions the client may currently see, written into `out`. */
  def possibleLocations(out: Box): Box =
    val cur = current
    if cur == null then out.setPoint(x, y, z)
    else
      cur.possibleLocations(out)
      val prev = previous
      if prev != null then out.include(prev.possibleLocations(scratch))
      out

  private def updateSpeed(relative: Boolean, preX: Double, preY: Double, preZ: Double): Unit =
    val nx = x
    val ny = y
    val nz = z
    if hasLastTarget then
      val decay = if steps <= 1 then 0.0 else (steps - 1.0) / steps
      decayInto(0, math.abs(nx - lastTargetX), decay)
      decayInto(1, math.abs(ny - lastTargetY), decay)
      decayInto(2, math.abs(nz - lastTargetZ), decay)
    hasLastTarget = true
    lastTargetX = nx; lastTargetY = ny; lastTargetZ = nz
    val jx = math.abs(nx - preX)
    val jy = math.abs(ny - preY)
    val jz = math.abs(nz - preZ)
    val lerped = jx < TeleportSnap && jy < TeleportSnap && jz < TeleportSnap
    jump = if relative || !lerped then null else Array(jx, jy, jz)

  private def decayInto(axis: Int, delta: Double, decay: Double): Unit =
    val decayed = speed(axis) * decay
    speed(axis) = if delta < SpeedSampleCap then math.max(decayed, delta) else decayed

  private def clampToSpeed(interp: Interpolation): Unit =
    val jitterXZ = if profile.coarsePositions then CoarseJitter else Jitter
    val jitterY = if profile.coarsePositions then CoarseJitterY else Jitter
    var ox = steps * math.max(speed(0), SpeedFloorXZ) * Safety + jitterXZ
    var oy = steps * math.max(speed(1), SpeedFloorY) * Safety + jitterY
    var oz = steps * math.max(speed(2), SpeedFloorXZ) * Safety + jitterXZ
    val j = jump
    if j != null then
      ox = math.max(ox, j(0) + jitterXZ)
      oy = math.max(oy, j(1) + jitterY)
      oz = math.max(oz, j(2) + jitterXZ)
    interp.clampStart(ox, oy, oz, teleport = j != null)

object TrackedEntity:
  private inline val Scale = 4096.0
  private inline def pack(v: Double): Long = math.round(v * Scale)

  private inline val SpeedFloorXZ = 0.05
  private inline val SpeedFloorY = 0.08
  private inline val Safety = 1.1
  private inline val Jitter = 1e-3
  private inline val CoarseJitter = 0.03125
  private inline val CoarseJitterY = 0.015625
  private inline val SpeedSampleCap = 8.0
  private inline val TeleportSnap = 64.0

  /** Interpolation steps the client uses for an entity type. */
  inline val LivingSteps = 3
  inline val MinecartSteps = 5
  inline val LegacyBoatSteps = 10
