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

package eigen.core.history

import eigen.core.schema.Schema
import eigen.core.schema.Schema.Col

/**
 * Recent per-tick frames of one player, stored column-major (structure of arrays) in a ring.
 *
 * A row is written in place through [[update]] and published by [[commit]]; nothing is
 * allocated per tick. Rows are addressed by a monotonically increasing sequence number, so
 * windows can be cut by sequence range and copied out with at most two `arraycopy` calls
 * per column — the layout the wire format uses, too.
 */
final class TickHistory(val capacity: Int):
  require(capacity >= 2 && Integer.bitCount(capacity) == 1, "capacity must be a power of two")

  private val mask = capacity - 1
  private val cols: Array[Array[Float]] = Array.fill(Schema.width)(new Array[Float](capacity))
  private val ticks = new Array[Int](capacity)
  private val epochs = new Array[Int](capacity)

  /** Sequence number of the row being written. Committed rows are `[oldest, next)`. */
  private var next: Long = 0

  inline def nextSeq: Long = next
  def oldestSeq: Long = math.max(0L, next - capacity)

  /** Writes `value` into column `col` of the row being built. */
  inline def update(col: Col, value: Float): Unit = cols(col.index)((next & mask).toInt) = value

  inline def update(col: Col, value: Boolean): Unit = update(col, if value then 1f else 0f)

  /** Publishes the row being built under the given client tick and epoch. */
  def commit(tick: Int, epoch: Int): Long =
    val slot = (next & mask).toInt
    ticks(slot) = tick
    epochs(slot) = epoch
    val seq = next
    next += 1
    // Clear the next slot so a column a system forgets to write reads as 0, never as stale data.
    val n = (next & mask).toInt
    var c = 0
    while c < cols.length do
      cols(c)(n) = 0f
      c += 1
    seq

  def holds(seq: Long): Boolean = seq >= oldestSeq && seq < next

  def apply(col: Col, seq: Long): Float =
    require(holds(seq), s"row $seq is not resident")
    cols(col.index)((seq & mask).toInt)

  /** Overwrites a committed value; used when a later system refines an earlier row. */
  def set(col: Col, seq: Long, value: Float): Unit =
    require(holds(seq), s"row $seq is not resident")
    cols(col.index)((seq & mask).toInt) = value

  def tickAt(seq: Long): Int = ticks((seq & mask).toInt)
  def epochAt(seq: Long): Int = epochs((seq & mask).toInt)

  /** True when `[from, from + rows)` is resident and lies inside a single epoch. */
  def contiguous(from: Long, rows: Int): Boolean =
    rows > 0 && from >= oldestSeq && from + rows <= next && epochAt(from) == epochAt(from + rows - 1)

  /** Copies `rows` values of `col` starting at sequence `from` into `dst` at `offset`. */
  def copyColumn(col: Col, from: Long, rows: Int, dst: Array[Float], offset: Int): Unit =
    val src = cols(col.index)
    val start = (from & mask).toInt
    val first = math.min(rows, capacity - start)
    System.arraycopy(src, start, dst, offset, first)
    if first < rows then System.arraycopy(src, 0, dst, offset + first, rows - first)
