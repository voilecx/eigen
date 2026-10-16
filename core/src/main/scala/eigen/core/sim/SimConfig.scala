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

package eigen.core.sim

/** Tuning of the per-player simulation; loaded from the plugin configuration. */
final case class SimConfig(
    /** Also treat non-player living entities as combat targets. */
    targetLiving: Boolean = false,
    /** A target is hittable within this distance, in blocks, from the eye. */
    gateRadius: Double = 8.0,
    /** …and within this angle, in degrees, of the look direction. */
    gateFov: Double = 75.0,
    /** Ticks around combat that are neither combat nor calm. */
    bufferTicks: Int = 60,
    attackPre: Int = 28,
    attackPost: Int = 12,
    /** Minimum ticks between two attack anchors. */
    attackMinGap: Int = 12,
    engagePre: Int = 12,
    engagePost: Int = 28,
    calmMinTicks: Int = 100,
    calmMaxTicks: Int = 1200,
    /** Fraction of calm ticks that must carry real mouse movement for a segment to be kept. */
    calmMinActive: Float = 0.25f,
    /** Target geometry is masked when its position uncertainty exceeds this, in blocks. */
    maxInflation: Double = 2.0,
    idleTicks: Int = 20,
    knockbackTicks: Int = 10,
    teleportTicks: Int = 2,
    /** Ticks a target stays selected after the player last hit it. */
    stickyTargetTicks: Int = 40,
):
  require(attackPre >= 0 && attackPost >= 1 && engagePre >= 0 && engagePost >= 1)
  require(calmMinTicks >= 1 && calmMaxTicks >= calmMinTicks)

  /** History must hold the delay line and the longest window. */
  def historyCapacity: Int =
    val need = math.max(bufferTicks, math.max(attackPre + attackPost, engagePre + engagePost)) + 8
    Integer.highestOneBit(need - 1) << 1
