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

package eigen.core

import scala.collection.mutable.ArrayBuffer

import eigen.core.aim.{AxisEstimator, RunningMode}
import eigen.core.geom.Angles
import eigen.core.latency.TransactionClock
import eigen.core.util.IntMap

class UtilSuite extends munit.FunSuite:

  test("wrapDegrees keeps angles in [-180, 180)"):
    assertEqualsDouble(Angles.wrapDegrees(190), -170, 1e-9)
    assertEqualsDouble(Angles.wrapDegrees(-190), 170, 1e-9)
    assertEqualsDouble(Angles.wrapDegrees(180), -180, 1e-9)
    assertEqualsDouble(Angles.turn(179, -179), 2, 1e-9)

  test("IntMap behaves like a map under random inserts and removals"):
    val map = IntMap[String]()
    val ref = scala.collection.mutable.HashMap[Int, String]()
    val rnd = scala.util.Random(7)
    for _ <- 0 until 20_000 do
      val k = rnd.nextInt(512) - 256
      if rnd.nextInt(3) == 0 then assertEquals(Option(map.remove(k)), ref.remove(k))
      else
        map(k) = k.toString
        ref(k) = k.toString
      assertEquals(map.size, ref.size)
    for k <- -256 until 256 do assertEquals(Option(map.getOrNull(k)), ref.get(k))
    var seen = 0
    map.foreachValue(_ => seen += 1)
    assertEquals(seen, ref.size)

  test("RunningMode tracks the most frequent bucket over a sliding window"):
    val mode = RunningMode(5)
    Seq(1.0, 1.0005, 2.0, 1.0, 3.0).foreach(mode.add(_, 1e-3))
    assertEqualsDouble(mode.mode, 1.0, 1e-9)
    assertEquals(mode.modeCount, 3)
    Seq(2.0, 2.0, 2.0).foreach(mode.add(_, 1e-3)) // evicts 1.0, 1.0005 and the first 2.0
    assertEqualsDouble(mode.mode, 2.0, 1e-9)
    assertEquals(mode.modeCount, 3)

  /** Rotation step of the client for a sensitivity in [0, 1]. */
  private def step(sensitivity: Double): Double =
    val f = sensitivity * 0.6 + 0.2
    f * f * f * 8 * 0.15

  test("AxisEstimator recovers the mouse step and whole-count dots"):
    val est = AxisEstimator()
    val s = step(0.5)
    val rnd = scala.util.Random(3)
    for _ <- 0 until 200 do
      val counts = (rnd.nextInt(30) + 1) * (if rnd.nextBoolean() then 1 else -1)
      est.observe(counts * s, math.ulp(100f).toDouble)
    assert(est.converged)
    assertEqualsDouble(est.currentStep, s, 1e-6)
    val dots = est.dots(7 * s)
    assertEqualsDouble(dots, 7.0, 1e-6)

  test("AxisEstimator starts over when the sensitivity changes"):
    val est = AxisEstimator()
    val rnd = scala.util.Random(5)
    def feed(s: Double, n: Int): Unit =
      for _ <- 0 until n do est.observe((rnd.nextInt(25) + 1) * s, 1e-6)
    feed(step(0.5), 200)
    assertEqualsDouble(est.currentStep, step(0.5), 1e-6)
    feed(step(0.83), 400)
    assert(est.converged)
    assertEqualsDouble(est.currentStep, step(0.83), 1e-6)

  test("TransactionClock runs all <= done tasks even when two listeners interleave [t, t+1, t, t+1]"):
    val clock = TransactionClock()
    val id1 = clock.nextId().toInt
    val id2 = clock.nextId().toInt
    clock.onPingSent(id1)
    clock.onPingSent(id2)
    assertEquals(clock.lastSent, 2)

    val order = ArrayBuffer.empty[String]
    // Listener A schedules first and second transaction tasks (e.g. Eigen on ENTITY_RELATIVE_MOVE)
    clock.at(1)(() => order += "eigen-t1")
    clock.at(2)(() => order += "eigen-t2")
    // Listener B schedules first and second transaction tasks (e.g. ShardCompat on ENTITY_RELATIVE_MOVE)
    clock.at(1)(() => order += "shard-t1")
    clock.at(2)(() => order += "shard-t2")

    assert(clock.onPong(id1))
    assertEquals(clock.lastReceived, 1)
    assertEquals(order.toList, List("eigen-t1", "shard-t1"))

    assert(clock.onPong(id2))
    assertEquals(clock.lastReceived, 2)
    assertEquals(order.toList, List("eigen-t1", "shard-t1", "eigen-t2", "shard-t2"))

  test("TransactionClock ignores foreign <= 0 ids (GrimAC) and unanswered ids"):
    val clock = TransactionClock()
    clock.onPingSent(0)
    clock.onPingSent(-1)
    clock.onPingSent(-32768)
    assertEquals(clock.lastSent, 0)
    assert(!clock.onPong(0))
    assert(!clock.onPong(-1))
    assert(!clock.onPong(42))
