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

import eigen.core.schema.Layout

/** Window kinds, as on the wire (protocol §5.1). */
enum WindowKind(val code: Int):
  case Attack extends WindowKind(1)
  case Engage extends WindowKind(2)
  case Calm extends WindowKind(3)

/**
 * A cut of a player's timeline, ready to encode. `data(i)` holds the rows of the i-th column of
 * `layout`. Routing fields (`handle`, `session`, `purpose`, `id`) are filled in by the transport.
 */
final class Window(
    val kind: WindowKind,
    val layout: Layout,
    val epoch: Int,
    val engagement: Int,
    val firstTick: Int,
    val rows: Int,
    val anchor: Int,
    val data: Array[Array[Float]],
):
  var handle: Int = 0
  var session: Int = 0
  var purpose: Int = 0
  var id: Int = 0

  def wireBytes: Int = Window.HeaderBytes + rows * layout.rowBytes

object Window:
  /** Size of the per-window header in a `WINDOWS` payload. */
  inline val HeaderBytes = 32

  inline val PurposeCollect = 1
  inline val PurposeInfer = 2

  /** Cuts `rows` rows starting at `from` out of `history` in the given layout. */
  def cut(
      history: TickHistory,
      kind: WindowKind,
      layout: Layout,
      from: Long,
      rows: Int,
      anchor: Int,
      engagement: Int,
  ): Window =
    val data = Array.ofDim[Float](layout.cols.length, rows)
    var i = 0
    while i < data.length do
      history.copyColumn(layout.cols(i), from, rows, data(i), 0)
      i += 1
    Window(kind, layout, history.epochAt(from), engagement, history.tickAt(from), rows, anchor, data)
