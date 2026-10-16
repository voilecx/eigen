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

/**
 * Why a tick's streams are masked; the bit set goes to the `reasons` meta column. Bits are part
 * of the dataset format: never renumber, only append.
 */
object Reason:
  inline val Teleport = 1 << 0
  inline val ServerRotation = 1 << 1
  inline val Uncalibrated = 1 << 2
  inline val Gui = 1 << 3
  inline val Vehicle = 1 << 4
  inline val Gliding = 1 << 5
  inline val Riptide = 1 << 6
  inline val Knockback = 1 << 7
  inline val Idle = 1 << 8
  inline val Uncertain = 1 << 9
  inline val NoTarget = 1 << 10
  inline val Water = 1 << 11
  /** Movement keys were inferred from motion (client older than 1.21.2), not reported. */
  inline val LegacyInput = 1 << 12
  inline val Discontinuity = 1 << 13

  inline val MasksRot = Teleport | ServerRotation | Uncalibrated | Gui | Vehicle | Gliding | Riptide | Idle | Discontinuity
  inline val MasksInp = Teleport | Gui | Vehicle | Gliding | Riptide | Knockback | Idle | Water
  inline val MasksTgt = NoTarget | Uncertain | Teleport | ServerRotation | Vehicle | Gliding | Riptide | Discontinuity

/** Bits of the `activity` meta column. */
object ActivityBit:
  inline val Moving = 1 << 0
  inline val Mining = 1 << 1
  inline val Building = 1 << 2
  inline val Rotating = 1 << 3
