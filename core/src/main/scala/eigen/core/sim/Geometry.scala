/*
 * This file is part of Eigen.
 * Copyright (C) 2026 Eigen contributors
 *
 * Portions of this file are derived from the following projects, also licensed under
 * the GNU General Public License version 3:
 *   Shard  - https://github.com/KaelusAI/Shard
 *            Copyright (C) 2026 KaelusAI
 *   GrimAC - https://github.com/GrimAnticheat/Grim
 *            Copyright (c) 2021-2026 GrimAC, DefineOutside and contributors
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

import eigen.core.geom.{Angles, Box, VanillaMath}

/** A unit look vector, mutable so it can live in a component and be reused every tick. */
final class Look3:
  var x = 0.0
  var y = 0.0
  var z = 1.0

  /** Sets this to the client's look vector for the given rotation (vanilla table trigonometry). */
  def fromRotation(yaw: Float, pitch: Float, doubleIndex: Boolean): Look3 =
    val pitchRad = (pitch * Angles.DegToRad).toFloat
    val yawRad = (-yaw * Angles.DegToRad).toFloat
    val cosPitch = VanillaMath.cos(pitchRad, doubleIndex)
    x = VanillaMath.sin(yawRad, doubleIndex) * cosPitch
    y = -VanillaMath.sin(pitchRad, doubleIndex)
    z = VanillaMath.cos(yawRad, doubleIndex) * cosPitch
    // The table leaves sin²+cos² off unity by up to 8e-5; normalise so angles near 0 stay exact.
    val len = math.sqrt(x * x + y * y + z * z)
    if len > 0 then { x /= len; y /= len; z /= len }
    this

/**
 * Aim geometry between an eye position and a target box. Pure functions over primitives and
 * caller-owned scratch objects: nothing allocates. Derived in part from Shard's ReachUtils
 * (GPL-3.0).
 */
object Geometry:

  /** Smallest angle, in degrees, between `look` and the directions to the box centre and corners. */
  def minAngleToBox(ex: Double, ey: Double, ez: Double, look: Look3, b: Box): Double =
    var best = angleTo(ex, ey, ez, look, b.centerX, b.centerY, b.centerZ)
    var corner = 0
    while corner < 8 do
      val px = if (corner & 1) == 0 then b.minX else b.maxX
      val py = if (corner & 2) == 0 then b.minY else b.maxY
      val pz = if (corner & 4) == 0 then b.minZ else b.maxZ
      best = math.min(best, angleTo(ex, ey, ez, look, px, py, pz))
      corner += 1
    // Looking at the box from inside or straight through its interior counts as 0.
    if rayHits(ex, ey, ez, look, 64.0, b) then 0.0 else best

  def angleTo(ex: Double, ey: Double, ez: Double, look: Look3, px: Double, py: Double, pz: Double): Double =
    val dx = px - ex
    val dy = py - ey
    val dz = pz - ez
    val len = math.sqrt(dx * dx + dy * dy + dz * dz)
    if len < 1e-3 then 0.0
    else
      val dot = (look.x * dx + look.y * dy + look.z * dz) / len
      math.acos(math.max(-1.0, math.min(1.0, dot))) * Angles.RadToDeg

  /**
   * Signed yaw and pitch error from the look direction to the box centre, each normalised by the
   * box's angular half-size, so ±1 is the edge of the target whatever its distance. Writes
   * `out(0)` (yaw) and `out(1)` (pitch).
   */
  def aimError(ex: Double, ey: Double, ez: Double, yaw: Float, pitch: Float, b: Box, out: Array[Double]): Unit =
    out(0) = 0; out(1) = 0
    val dx = b.centerX - ex
    val dy = b.centerY - ey
    val dz = b.centerZ - ez
    val horizontal = math.sqrt(dx * dx + dz * dz)
    val distance = math.sqrt(dx * dx + dy * dy + dz * dz)
    if distance < 1e-3 then return
    val deltaYaw = Angles.wrapDegrees(yaw - math.atan2(-dx, dz) * Angles.RadToDeg)
    val deltaPitch = pitch - (-math.atan2(dy, horizontal) * Angles.RadToDeg)
    val halfWidth = math.max(b.sizeX, b.sizeZ) / 2
    val angularWidth = math.atan2(halfWidth, distance) * Angles.RadToDeg
    val angularHeight = math.atan2(b.sizeY / 2, distance) * Angles.RadToDeg
    if angularWidth > 1e-6 then out(0) = deltaYaw / angularWidth
    if angularHeight > 1e-6 then out(1) = deltaPitch / angularHeight

  /**
   * Intersects the ray `eye + t * look`, `0 <= t <= reach`, with `b`. On a hit writes the hit point
   * into `hit(0..2)` and returns the distance; returns -1 on a miss. Slab method.
   */
  def rayHit(ex: Double, ey: Double, ez: Double, look: Look3, reach: Double, b: Box, hit: Array[Double]): Double =
    var tMin = 0.0
    var tMax = reach
    inline def slab(o: Double, d: Double, lo: Double, hi: Double): Boolean =
      if math.abs(d) < 1e-12 then o >= lo && o <= hi
      else
        val t1 = (lo - o) / d
        val t2 = (hi - o) / d
        tMin = math.max(tMin, math.min(t1, t2))
        tMax = math.min(tMax, math.max(t1, t2))
        tMin <= tMax
    if slab(ex, look.x, b.minX, b.maxX) && slab(ey, look.y, b.minY, b.maxY) && slab(ez, look.z, b.minZ, b.maxZ) then
      hit(0) = ex + look.x * tMin
      hit(1) = ey + look.y * tMin
      hit(2) = ez + look.z * tMin
      tMin
    else -1.0

  def rayHits(ex: Double, ey: Double, ez: Double, look: Look3, reach: Double, b: Box): Boolean =
    var tMin = 0.0
    var tMax = reach
    // Inline so the slab bounds stay locals instead of captured, heap-boxed vars.
    inline def slab(o: Double, d: Double, lo: Double, hi: Double): Boolean =
      if math.abs(d) < 1e-12 then o >= lo && o <= hi
      else
        val t1 = (lo - o) / d
        val t2 = (hi - o) / d
        tMin = math.max(tMin, math.min(t1, t2))
        tMax = math.min(tMax, math.max(t1, t2))
        tMin <= tMax
    slab(ex, look.x, b.minX, b.maxX) && slab(ey, look.y, b.minY, b.maxY) && slab(ez, look.z, b.minZ, b.maxZ)

  /** Closest approach between the ray segment and the box, sampled along the segment. */
  def missDistance(ex: Double, ey: Double, ez: Double, look: Look3, reach: Double, b: Box): Double =
    var best = Double.MaxValue
    var i = 0
    while i <= MissSamples do
      val t = reach * i / MissSamples
      best = math.min(best, b.distanceTo(ex + look.x * t, ey + look.y * t, ez + look.z * t))
      i += 1
    best

  private inline val MissSamples = 32
