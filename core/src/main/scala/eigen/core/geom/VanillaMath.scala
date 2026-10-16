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

package eigen.core.geom

/** The client's lookup-table trigonometry, so look vectors match what the client computes. */
object VanillaMath:

  private inline val TableSize = 65536
  private inline val TableMask = 65535
  private inline val CosOffset = 16384.0
  private inline val Scale = 10430.378350470453
  private inline val LegacyScale = 10430.378f

  // sin(i * 2pi / 65536), identical to the client's table in float32.
  private val Sin: Array[Float] = Array.tabulate(TableSize)(i => math.sin(i / Scale).toFloat)

  /** @param doubleIndex clients 1.21.11+ index the table in double precision. */
  def sin(radians: Float, doubleIndex: Boolean): Float = Sin(index(radians, 0.0, doubleIndex))

  def cos(radians: Float, doubleIndex: Boolean): Float = Sin(index(radians, CosOffset, doubleIndex))

  private inline def index(radians: Float, offset: Double, doubleIndex: Boolean): Int =
    if doubleIndex then ((radians * Scale + offset).toLong & TableMask).toInt
    else (radians * LegacyScale + offset.toFloat).toInt & TableMask
