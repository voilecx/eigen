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

package eigen.core.util

import scala.reflect.ClassTag

/**
 * Open-addressing hash map from `Int` keys to references, with linear probing and
 * backward-shift deletion. No boxing of keys and no allocation on lookup; iteration walks
 * the backing arrays. Not thread-safe: owned by a single player's network thread.
 */
final class IntMap[A <: AnyRef: ClassTag](initialCapacity: Int = 16):
  private var keys = new Array[Int](IntMap.capacityFor(initialCapacity))
  private var vals = new Array[A](keys.length)
  private var used = 0

  def size: Int = used
  def isEmpty: Boolean = used == 0

  /** Returns the value for `key`, or `null` when absent. */
  def getOrNull(key: Int): A =
    val mask = keys.length - 1
    var i = IntMap.mix(key) & mask
    while vals(i) != null do
      if keys(i) == key then return vals(i)
      i = (i + 1) & mask
    null.asInstanceOf[A]

  def contains(key: Int): Boolean = getOrNull(key) != null

  def update(key: Int, value: A): Unit =
    require(value != null, "IntMap does not store nulls")
    if (used + 1) * 4 > keys.length * 3 then grow()
    val mask = keys.length - 1
    var i = IntMap.mix(key) & mask
    while vals(i) != null && keys(i) != key do i = (i + 1) & mask
    if vals(i) == null then used += 1
    keys(i) = key
    vals(i) = value

  def remove(key: Int): A =
    val mask = keys.length - 1
    var i = IntMap.mix(key) & mask
    while vals(i) != null && keys(i) != key do i = (i + 1) & mask
    val removed = vals(i)
    if removed == null then return removed
    used -= 1
    // Backward-shift deletion keeps probe chains intact without tombstones.
    var gap = i
    var j = (i + 1) & mask
    while vals(j) != null do
      val home = IntMap.mix(keys(j)) & mask
      val movable = if gap <= j then home <= gap || home > j else home <= gap && home > j
      if movable then
        keys(gap) = keys(j); vals(gap) = vals(j); gap = j
      j = (j + 1) & mask
    vals(gap) = null.asInstanceOf[A]
    removed

  def clear(): Unit =
    java.util.Arrays.fill(vals.asInstanceOf[Array[AnyRef]], null)
    used = 0

  inline def foreachValue(inline f: A => Unit): Unit =
    val vs = values
    var i = 0
    while i < vs.length do
      val v = vs(i)
      if v != null then f(v)
      i += 1

  /** Backing value array, for allocation-free iteration (`null` slots are empty). */
  def values: Array[A] = vals

  private def grow(): Unit =
    val oldKeys = keys
    val oldVals = vals
    keys = new Array[Int](oldKeys.length * 2)
    vals = new Array[A](keys.length)
    used = 0
    var i = 0
    while i < oldKeys.length do
      if oldVals(i) != null then update(oldKeys(i), oldVals(i))
      i += 1

object IntMap:
  private def capacityFor(n: Int): Int = Integer.highestOneBit(math.max(4, n * 4 / 3) * 2 - 1)

  /** Murmur3 finaliser: spreads sequential entity ids over the table. */
  private inline def mix(k: Int): Int =
    var h = k
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^ (h >>> 16)
