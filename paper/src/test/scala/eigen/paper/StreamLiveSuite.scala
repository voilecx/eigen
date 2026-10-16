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

package eigen.paper

import java.net.URI
import java.util.logging.Logger

import eigen.core.history.{Window, WindowKind}
import eigen.core.schema.Schema
import eigen.core.wire.Messages
import eigen.paper.config.BackendSettings
import eigen.paper.stream.{StreamClient, StreamState}

/**
 * End-to-end check against a running backend. Skipped unless `EIGEN_TEST_BACKEND`
 * (e.g. `ws://127.0.0.1:8787/v1/stream`) and `EIGEN_TEST_TOKEN` are set.
 */
class StreamLiveSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  private val backend = sys.env.get("EIGEN_TEST_BACKEND")
  private val token = sys.env.get("EIGEN_TEST_TOKEN")

  private def window(kind: WindowKind, rows: Int): Window =
    val layout = if kind == WindowKind.Calm then Schema.Calm else Schema.Combat
    val data = Array.tabulate(layout.cols.length, rows)((c, r) => if layout.cols(c).tpe.size == 4 then c + r * 0.5f else (r % 2).toFloat)
    Window(kind, layout, epoch = 1, engagement = if kind == WindowKind.Calm then 0 else 1, firstTick = 500, rows, anchor = if kind == WindowKind.Calm then -1 else rows / 2, data)

  private def awaitUntil(what: String)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + 20_000
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(50)
    assert(cond, s"timed out waiting for $what")

  test("streams players, a session and windows, and every frame is acknowledged"):
    assume(backend.isDefined && token.isDefined, "EIGEN_TEST_BACKEND / EIGEN_TEST_TOKEN not set")
    val client = StreamClient(BackendSettings(enabled = true, URI(backend.get), token.get, 64L << 20), "eigen test", "munit", Logger.getLogger("test"))
    client.start()
    try
      awaitUntil("connection")(client.state == StreamState.Ready)
      val player = "ab" * 32
      client.playerJoined(Messages.PlayerJoin(1, player, 773, "vanilla"))
      client.sessionStarted(Messages.CollectStart(1, java.util.UUID.randomUUID().toString, 1, "legit", Map("client" -> "vanilla"), "live test", System.currentTimeMillis()))
      for i <- 0 until 50 do
        val w = window(if i % 5 == 0 then WindowKind.Calm else WindowKind.Attack, if i % 5 == 0 then 300 else 40)
        w.handle = 1
        w.session = 1
        w.purpose = Window.PurposeCollect
        client.window(w)
      client.mark(Messages.Mark(1, 520, "on", "toggle"))
      client.sessionStopped(Messages.CollectStop(1, System.currentTimeMillis()))
      client.playerLeft(1)
      awaitUntil("acknowledgements")(client.backlog == 0 && client.unacknowledged == 0)
      assertEquals(client.state, StreamState.Ready)
      assertEquals(client.dropped, 0L)
    finally client.stop()
