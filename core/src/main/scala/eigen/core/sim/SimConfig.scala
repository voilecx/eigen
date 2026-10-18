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
    /** Ticks after the last hit that still count as combat. Combat starts only with a hit. */
    combatTicks: Int = 60,
    /** Ticks around combat that are neither combat nor calm. */
    bufferTicks: Int = 60,
    attackPre: Int = 56,
    attackPost: Int = 24,
    /** Minimum ticks between two attack anchors. */
    attackMinGap: Int = 20,
    engagePre: Int = 24,
    engagePost: Int = 56,
    calmMinTicks: Int = 100,
    calmMaxTicks: Int = 1200,
    /** Fraction of calm ticks that must carry real mouse movement for a segment to be kept. */
    calmMinActive: Float = 0.25f,
    /** Target geometry is masked when its position uncertainty exceeds this, in blocks. */
    maxInflation: Double = 2.0,
    idleTicks: Int = 20,
    knockbackTicks: Int = 10,
    teleportTicks: Int = 2,
):
  require(attackPre >= 0 && attackPost >= 1 && engagePre >= 0 && engagePost >= 1)
  require(calmMinTicks >= 1 && calmMaxTicks >= calmMinTicks)

  /** History must hold the delay line and the longest window. */
  def historyCapacity: Int =
    val need = math.max(bufferTicks, math.max(attackPre + attackPost, engagePre + engagePost)) + 8
    Integer.highestOneBit(need - 1) << 1
