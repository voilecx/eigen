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
import eigen.core.wire.ByteSink

/**
 * Accumulates one contiguous run of calm (non-combat) ticks in the low-dimensional calm layout.
 *
 * Memory is bounded by `maxRows`: a run that reaches it is emitted and a new one starts, so a
 * player who never fights produces a sequence of segments rather than an ever-growing buffer.
 * Runs shorter than `minRows`, or with too few ticks of real mouse movement, are discarded.
 */
final class CalmTrack(minRows: Int, maxRows: Int, minActive: Float):
  require(minRows >= 1 && maxRows >= minRows)

  private val layout = Schema.Calm
  private val cols: Array[Array[Float]] = Array.fill(layout.cols.length)(new Array[Float](maxRows))
  // A tick counts as active when the mouse actually moved, not merely when rotation is unmasked.
  private val yawCol = layout.cols.indexOf(Schema.DotsYaw)
  private val pitchCol = layout.cols.indexOf(Schema.DotsPitch)

  private var rows = 0
  private var active = 0
  private var firstTick = 0
  private var epoch = 0
  private var engagement = 0
  private var lastSeq = -1L

  def length: Int = rows

  /**
   * Appends committed row `seq`. Returns a finished segment when this row closes one (the
   * buffer is full); a row that does not continue the current run starts a new one, so call
   * [[flush]] first when the caller knows the run ended.
   */
  def append(history: TickHistory, seq: Long, engagementNow: Int): Window | Null =
    val handedOff = rows > 0 && lastSeq == -1L
    val continues = rows > 0 && (handedOff || (seq == lastSeq + 1 && history.epochAt(seq) == epoch))
    val closed = if rows > 0 && !continues then flush() else null
    if rows == 0 then
      firstTick = history.tickAt(seq)
      epoch = history.epochAt(seq)
      engagement = engagementNow
    else if handedOff then
      firstTick = math.max(0, history.tickAt(seq) - rows)
      epoch = history.epochAt(seq)
    var i = 0
    while i < cols.length do
      cols(i)(rows) = history(layout.cols(i), seq)
      i += 1
    if cols(yawCol)(rows) != 0f || cols(pitchCol)(rows) != 0f then active += 1
    rows += 1
    lastSeq = seq
    if closed != null then closed
    else if rows == maxRows then flush()
    else null

  /** Ends the current run; returns it when it is long and active enough to keep. */
  def flush(): Window | Null =
    val n = rows
    val keep = n >= minRows && active >= minActive * n
    val out =
      if !keep then null
      else
        val data = Array.ofDim[Float](cols.length, n)
        var i = 0
        while i < cols.length do
          System.arraycopy(cols(i), 0, data(i), 0, n)
          i += 1
        Window(WindowKind.Calm, layout, epoch, engagement, firstTick, n, -1, data)
    rows = 0
    active = 0
    lastSeq = -1
    out

  def writeTo(sink: ByteSink): Unit =
    sink.u16(rows)
    sink.u16(active)
    sink.u32(engagement.toLong & 0xffffffffL)
    var c = 0
    while c < cols.length do
      var r = 0
      while r < rows do
        sink.f32(cols(c)(r))
        r += 1
      c += 1

  def readFrom(bytes: Array[Byte], at: Int): Int =
    if at + 8 > bytes.length then return -1
    val savedRows = ByteSink.readU16(bytes, at)
    val savedActive = ByteSink.readU16(bytes, at + 2)
    val savedEngagement = ByteSink.readI32(bytes, at + 4)
    if savedRows > 4096 || savedActive > savedRows || savedEngagement < 0 then return -1
    val end = at + 8 + savedRows * cols.length * 4
    if end > bytes.length then return -1
    if rows == 0 && savedRows > 0 then
      val keep = math.min(savedRows, maxRows)
      val skip = savedRows - keep
      var c = 0
      while c < cols.length do
        val colOff = at + 8 + (c * savedRows + skip) * 4
        var r = 0
        while r < keep do
          val v = ByteSink.readF32(bytes, colOff + r * 4)
          if !java.lang.Float.isFinite(v) then return -1
          cols(c)(r) = v
          r += 1
        c += 1
      var act = 0
      var r = 0
      while r < keep do
        if cols(yawCol)(r) != 0f || cols(pitchCol)(r) != 0f then act += 1
        r += 1
      rows = keep
      active = act
      engagement = savedEngagement
      firstTick = 0
      epoch = 1
      lastSeq = -1L
    end
