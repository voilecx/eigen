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

package eigen.core.aim

import eigen.core.wire.ByteSink

/**
 * Mode of the last `window` samples, where samples closer than a bucket width count as equal.
 * Flat arrays instead of a map: the number of distinct buckets is small and bounded by `window`.
 */
final class RunningMode(window: Int):
  private val history = new Array[Int](window) // bucket index of each sample, ring
  private var head = 0
  private var filled = 0

  private val keys = new Array[Double](window)
  private val counts = new Array[Int](window)
  private var buckets = 0

  def size: Int = filled

  def add(value: Double, width: Double): Unit =
    if filled == window then evict()
    var b = 0
    while b < buckets && math.abs(keys(b) - value) >= width do b += 1
    if b == buckets then
      keys(b) = value
      counts(b) = 0
      buckets += 1
    counts(b) += 1
    history((head + filled) % window) = b
    filled += 1

  /** The most popular bucket's value and count; `(0, 0)` when empty. */
  def mode: Double =
    val b = best
    if b < 0 then 0.0 else keys(b)

  def modeCount: Int =
    val b = best
    if b < 0 then 0 else counts(b)

  def clear(): Unit =
    head = 0; filled = 0; buckets = 0

  def writeTo(sink: ByteSink): Unit =
    sink.u16(filled)
    sink.u16(buckets)
    var b = 0
    while b < buckets do
      sink.f64(keys(b))
      sink.u16(counts(b))
      b += 1
    var i = 0
    while i < filled do
      sink.u8(history((head + i) % window))
      i += 1

  def readFrom(bytes: Array[Byte], at: Int): Int =
    if at + 4 > bytes.length then return -1
    val f = ByteSink.readU16(bytes, at)
    val bCount = ByteSink.readU16(bytes, at + 2)
    if f > window || bCount > window || (f == 0) != (bCount == 0) then return -1
    val end = at + 4 + bCount * 10 + f
    if end > bytes.length then return -1
    var pos = at + 4
    var sum = 0
    var b = 0
    while b < bCount do
      val k = ByteSink.readF64(bytes, pos)
      val c = ByteSink.readU16(bytes, pos + 8)
      if !java.lang.Double.isFinite(k) || k <= 0.0 || c <= 0 || c > f then return -1
      keys(b) = k
      counts(b) = c
      sum += c
      pos += 10
      b += 1
    if sum != f then return -1
    var i = 0
    while i < f do
      val idx = bytes(pos + i) & 0xff
      if idx >= bCount then return -1
      history(i) = idx
      i += 1
    head = 0
    filled = f
    buckets = bCount
    end

  private def best: Int =
    var b = -1
    var i = 0
    while i < buckets do
      if b < 0 || counts(i) > counts(b) then b = i
      i += 1
    b

  private def evict(): Unit =
    val b = history(head)
    head = (head + 1) % window
    filled -= 1
    counts(b) -= 1
    if counts(b) == 0 then
      // Move the last bucket into the hole and repoint its samples.
      val last = buckets - 1
      if b != last then
        keys(b) = keys(last)
        counts(b) = counts(last)
        var i = 0
        while i < filled do
          val s = (head + i) % window
          if history(s) == last then history(s) = b
          i += 1
      buckets -= 1
