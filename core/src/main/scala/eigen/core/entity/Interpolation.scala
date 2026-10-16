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

/**
 * Where the client may currently render an entity that the server moved, while the client
 * interpolates from the old position to the new one over `steps` ticks.
 *
 * The client's progress is only known within bounds: `low` is the fewest interpolation ticks it
 * can have run, `high` the most. The possible location is the union of the intermediate boxes
 * between those bounds. Instances are recycled by [[TrackedEntity]]; nothing here allocates.
 * Derived from GrimAC's reach interpolation (GPL-3.0).
 */
final class Interpolation:
  val start: Box = Box()
  val target: Box = Box()
  private var steps = 1
  private var low = 0
  private var high = 0
  private var unobservedTicks = false

  /**
   * Starts interpolating from `from` towards the point `(x, y, z)` over `steps` ticks.
   * `unobservedTicks` is true for clients that can tick without sending anything; their upper
   * bound is then unknown and runs straight to the end.
   */
  def reset(from: Box, x: Double, y: Double, z: Double, steps: Int, unobservedTicks: Boolean): Interpolation =
    start.copyFrom(from)
    target.setPoint(x, y, z)
    this.steps = math.max(1, steps)
    this.unobservedTicks = unobservedTicks
    low = 0
    high = if unobservedTicks then this.steps else 0
    this

  /** A move that does not interpolate (the entity stays wherever it possibly was). */
  def hold(from: Box): Interpolation =
    start.copyFrom(from)
    target.copyFrom(from)
    steps = 1
    unobservedTicks = false
    low = 0
    high = 0
    this

  /** Advances one client tick. `lowAdvances` is false while an older move is still settling. */
  def tick(lowAdvances: Boolean): Unit =
    if lowAdvances then low = math.min(low + 1, steps)
    high = if unobservedTicks then steps else math.min(high + 1, steps)

  /** Union of the positions the entity's feet can be at, written into `out`. */
  def possibleLocations(out: Box): Box =
    val inv = 1.0 / steps
    val sMinX = (target.minX - start.minX) * inv
    val sMaxX = (target.maxX - start.maxX) * inv
    val sMinY = (target.minY - start.minY) * inv
    val sMaxY = (target.maxY - start.maxY) * inv
    val sMinZ = (target.minZ - start.minZ) * inv
    val sMaxZ = (target.maxZ - start.maxZ) * inv
    // Intermediate boxes move linearly, so the union over steps low..high is spanned by its ends.
    out.minX = math.min(start.minX + low * sMinX, start.minX + high * sMinX)
    out.minY = math.min(start.minY + low * sMinY, start.minY + high * sMinY)
    out.minZ = math.min(start.minZ + low * sMinZ, start.minZ + high * sMinZ)
    out.maxX = math.max(start.maxX + low * sMaxX, start.maxX + high * sMaxX)
    out.maxY = math.max(start.maxY + low * sMaxY, start.maxY + high * sMaxY)
    out.maxZ = math.max(start.maxZ + low * sMaxZ, start.maxZ + high * sMaxZ)
    out

  /**
   * Bounds the start box around the target: an entity can not have been further away than
   * `steps` times its observed speed, plus jitter. Keeps an old, wide uncertainty from leaking
   * into every later move.
   */
  def clampStart(offX: Double, offY: Double, offZ: Double, teleport: Boolean): Unit =
    inline def lo(min: Double, max: Double, c: Double, off: Double): Double =
      val a = math.max(min, c - off)
      if a <= math.min(max, c + off) then a else if teleport then min else c
    inline def hi(min: Double, max: Double, c: Double, off: Double): Double =
      val b = math.min(max, c + off)
      if math.max(min, c - off) <= b then b else if teleport then max else c
    val cx = target.centerX
    val cy = target.centerY
    val cz = target.centerZ
    val x0 = lo(start.minX, start.maxX, cx, offX); val x1 = hi(start.minX, start.maxX, cx, offX)
    val y0 = lo(start.minY, start.maxY, cy, offY); val y1 = hi(start.minY, start.maxY, cy, offY)
    val z0 = lo(start.minZ, start.maxZ, cz, offZ); val z1 = hi(start.minZ, start.maxZ, cz, offZ)
    start.minX = x0; start.maxX = x1
    start.minY = y0; start.maxY = y1
    start.minZ = z0; start.maxZ = z1
