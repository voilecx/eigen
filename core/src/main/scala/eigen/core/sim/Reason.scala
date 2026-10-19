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
 *
 * Encapsulated as a zero-cost `opaque type` so bitset combinations cannot be confused with
 * arbitrary integers or activity bits while compiling down to unboxed `int` instructions.
 */
object Reason:
  opaque type Reason = Int

  val None: Reason = 0
  val Teleport: Reason = 1 << 0
  val ServerRotation: Reason = 1 << 1
  val Uncalibrated: Reason = 1 << 2
  val Gui: Reason = 1 << 3
  val Vehicle: Reason = 1 << 4
  val Gliding: Reason = 1 << 5
  val Riptide: Reason = 1 << 6
  val Knockback: Reason = 1 << 7
  val Idle: Reason = 1 << 8
  val Uncertain: Reason = 1 << 9
  val NoTarget: Reason = 1 << 10
  val Water: Reason = 1 << 11
  /** Movement keys were inferred from motion (client older than 1.21.2), not reported. */
  val LegacyInput: Reason = 1 << 12
  val Discontinuity: Reason = 1 << 13
  /** 1.21.2+ only: the tick was closed by `CLIENT_TICK_END` with no movement packet, so a
    * client without tick-end packets would not have produced this row. An observation marker,
    * deliberately outside the validity masks: training can filter on it to compare client
    * versions on equal terms. */
  val NoMoveTick: Reason = 1 << 14
  /** An attack input happened this tick, whether or not a target was matched; the `attack`
    * feature column stays target-gated, this bit marks the raw input. Last free bit of the
    * U16: the next reason needs a wider column. */
  val Attack: Reason = 1 << 15

  val MasksRot: Reason = Teleport | ServerRotation | Uncalibrated | Gui | Vehicle | Gliding | Riptide | Idle | Discontinuity
  val MasksInp: Reason = Teleport | Gui | Vehicle | Gliding | Riptide | Knockback | Idle | Water
  val MasksTgt: Reason = NoTarget | Uncertain | Teleport | ServerRotation | Vehicle | Gliding | Riptide | Discontinuity

  inline def fromRaw(bits: Int): Reason = bits

  extension (self: Reason)
    inline def |(other: Reason): Reason = self | other
    inline def &(other: Reason): Reason = self & other
    inline def intersects(mask: Reason): Boolean = (self & mask) != 0
    inline def disjointFrom(mask: Reason): Boolean = (self & mask) == 0
    inline def contains(bit: Reason): Boolean = (self & bit) == bit
    inline def when(cond: Boolean): Reason = if cond then self else 0
    inline def raw: Int = self
    inline def toInt: Int = self
    inline def toFloat: Float = self.toFloat

/** Bits of the `activity` meta column, encapsulated as a zero-cost `opaque type`. */
object ActivityBit:
  opaque type Activity = Int

  val None: Activity = 0
  val Moving: Activity = 1 << 0
  val Mining: Activity = 1 << 1
  val Building: Activity = 1 << 2
  val Rotating: Activity = 1 << 3

  inline def fromRaw(bits: Int): Activity = bits

  extension (self: Activity)
    inline def |(other: Activity): Activity = self | other
    inline def &(other: Activity): Activity = self & other
    inline def intersects(mask: Activity): Boolean = (self & mask) != 0
    inline def contains(bit: Activity): Boolean = (self & bit) == bit
    inline def when(cond: Boolean): Activity = if cond then self else 0
    inline def raw: Int = self
    inline def toInt: Int = self
    inline def toFloat: Float = self.toFloat