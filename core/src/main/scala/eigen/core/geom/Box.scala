/*
 * This file is part of Eigen.
 * Copyright (C) 2026 Eigen contributors
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

package eigen.core.geom

/** Mutable axis-aligned box. Used as scratch space on the hot path to avoid allocation. */
final class Box(
    var minX: Double = 0,
    var minY: Double = 0,
    var minZ: Double = 0,
    var maxX: Double = 0,
    var maxY: Double = 0,
    var maxZ: Double = 0,
):

  def set(x0: Double, y0: Double, z0: Double, x1: Double, y1: Double, z1: Double): Box =
    minX = math.min(x0, x1); maxX = math.max(x0, x1)
    minY = math.min(y0, y1); maxY = math.max(y0, y1)
    minZ = math.min(z0, z1); maxZ = math.max(z0, z1)
    this

  def setPoint(x: Double, y: Double, z: Double): Box = set(x, y, z, x, y, z)

  def copyFrom(o: Box): Box = set(o.minX, o.minY, o.minZ, o.maxX, o.maxY, o.maxZ)

  /** Grows this box to also cover `o`. */
  def include(o: Box): Box =
    minX = math.min(minX, o.minX); maxX = math.max(maxX, o.maxX)
    minY = math.min(minY, o.minY); maxY = math.max(maxY, o.maxY)
    minZ = math.min(minZ, o.minZ); maxZ = math.max(maxZ, o.maxZ)
    this

  def expand(x: Double, y: Double, z: Double): Box =
    minX -= x; maxX += x
    minY -= y; maxY += y
    minZ -= z; maxZ += z
    this

  /** Turns a box of possible feet positions into a box of possible hitboxes. */
  def growByDimensions(width: Double, height: Double): Box =
    val half = width / 2
    minX -= half; maxX += half
    minZ -= half; maxZ += half
    maxY += height
    this

  inline def centerX: Double = (minX + maxX) * 0.5
  inline def centerY: Double = (minY + maxY) * 0.5
  inline def centerZ: Double = (minZ + maxZ) * 0.5
  inline def sizeX: Double = maxX - minX
  inline def sizeY: Double = maxY - minY
  inline def sizeZ: Double = maxZ - minZ

  /** Distance from a point to the closest point of the box (0 when inside). */
  def distanceTo(x: Double, y: Double, z: Double): Double =
    import Box.clamp
    val dx = x - clamp(x, minX, maxX)
    val dy = y - clamp(y, minY, maxY)
    val dz = z - clamp(z, minZ, maxZ)
    math.sqrt(dx * dx + dy * dy + dz * dz)

  override def toString: String = f"Box($minX%.3f,$minY%.3f,$minZ%.3f -> $maxX%.3f,$maxY%.3f,$maxZ%.3f)"

object Box:
  inline def clamp(v: Double, lo: Double, hi: Double): Double = if v < lo then lo else if v > hi then hi else v
