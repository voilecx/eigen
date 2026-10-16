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

import eigen.core.entity.{ClientProfile, EntityKind, TrackedEntity}
import eigen.core.history.{Window, WindowKind}
import eigen.core.latency.TransactionClock
import eigen.core.schema.Schema
import eigen.core.sim.{PlayerSim, Reason, SimConfig}

class SimSuite extends munit.FunSuite:

  private val step = { val f = 0.5 * 0.6 + 0.2; f * f * f * 8 * 0.15 }

  private final class Harness(cfg: SimConfig = SimConfig(calmMinTicks = 40, calmMaxTicks = 200)):
    val windows = ArrayBuffer[Window]()
    val sim = PlayerSim(ClientProfile.Modern, cfg, windows += _)
    sim.windowsWanted = true
    sim.selfId = 1
    private var yaw = 0f
    private var pitch = 0f
    private val rnd = scala.util.Random(11)

    /** One client tick with a random whole-count mouse movement. */
    def tick(attack: Int = -1, swing: Boolean = false): Unit =
      yaw += ((rnd.nextInt(9) - 4) * step).toFloat
      pitch = math.max(-30f, math.min(30f, pitch + ((rnd.nextInt(5) - 2) * step).toFloat))
      sim.onFlying(hasPos = true, 0, 64, 0, hasRot = true, yaw, pitch, onGround = true)
      if attack >= 0 then sim.onAttack(attack)
      if swing || attack >= 0 then sim.onSwing()
      sim.onTickEnd()

    def spawnTarget(id: Int, z: Double): TrackedEntity =
      val e = TrackedEntity(id, EntityKind.Player, TrackedEntity.LivingSteps, ClientProfile.Modern)
      e.spawnAt(0, 64, z)
      sim.addEntity(e)
      e

  test("calm play produces a calm segment linked to no engagement"):
    val h = Harness()
    for _ <- 0 until 400 do h.tick()
    val calm = h.windows.filter(_.kind == WindowKind.Calm)
    assert(calm.nonEmpty, "expected calm segments")
    calm.foreach: w =>
      assertEquals(w.layout, Schema.Calm)
      assertEquals(w.engagement, 0)
      assertEquals(w.anchor, -1)
      assert(w.rows <= 200)
    assert(h.windows.forall(_.kind == WindowKind.Calm))

  test("an attack cuts a fixed-length combat window anchored on the hit"):
    val cfg = SimConfig(calmMinTicks = 40, calmMaxTicks = 200)
    val h = Harness(cfg)
    for _ <- 0 until 150 do h.tick() // calibrate the mouse estimator
    h.spawnTarget(2, 2.5)
    for _ <- 0 until 30 do h.tick()
    h.tick(attack = 2)
    for _ <- 0 until cfg.engagePost do h.tick()

    val attack = h.windows.filter(_.kind == WindowKind.Attack)
    assertEquals(attack.size, 1)
    val w = attack.head
    assertEquals(w.rows, cfg.attackPre + cfg.attackPost)
    assertEquals(w.anchor, cfg.attackPre)
    assertEquals(w.layout, Schema.Combat)
    val attackCol = Schema.Combat.cols.indexOf(Schema.Attack)
    assertEquals(w.data(attackCol)(w.anchor), 1f)
    val maskTgt = Schema.Combat.cols.indexOf(Schema.MaskTgt)
    assertEquals(w.data(maskTgt)(w.anchor), 1f, "target geometry valid at the hit")
    val reasons = Schema.Combat.cols.indexOf(Schema.Reasons)
    assertEquals(w.data(reasons)(w.anchor).toInt & Reason.Uncalibrated, 0)

    val engage = h.windows.filter(_.kind == WindowKind.Engage)
    assertEquals(engage.size, 1)
    assertEquals(engage.head.engagement, 1)
    assertEquals(w.engagement, 1)

  test("calm after a fight carries the engagement that preceded it"):
    val h = Harness()
    for _ <- 0 until 150 do h.tick()
    val target = h.spawnTarget(2, 2.5)
    for _ <- 0 until 20 do h.tick(attack = 2)
    target.dead = true
    for _ <- 0 until 500 do h.tick()
    val before = h.windows.filter(w => w.kind == WindowKind.Calm && w.engagement == 0)
    val after = h.windows.filter(w => w.kind == WindowKind.Calm && w.engagement == 1)
    assert(before.nonEmpty && after.nonEmpty, s"calm before=${before.size} after=${after.size}")
    assert(after.forall(_.firstTick > before.map(_.firstTick).max))

  test("a broken epoch never ends up inside one window"):
    val h = Harness()
    for _ <- 0 until 120 do h.tick()
    h.sim.breakEpoch()
    for _ <- 0 until 300 do h.tick()
    val epochs = h.windows.map(_.epoch).toSet
    assert(epochs.contains(2))
    h.windows.foreach(w => assert(w.epoch == 1 || w.epoch == 2))

  test("TransactionClock runs tasks in order once their pong arrives and ignores foreign ids"):
    val clock = TransactionClock()
    val ran = ArrayBuffer[Int]()
    val a = clock.nextId(); clock.onPingSent(a)
    clock.at(clock.lastSent)(() => ran += 1)
    val b = clock.nextId(); clock.onPingSent(b)
    clock.at(clock.lastSent)(() => ran += 2)
    assert(!clock.onPong(-5), "GrimAC's ids are not ours")
    assert(!clock.onPong(70000), "ids wider than a short are not ours")
    assert(clock.onPong(a))
    assertEquals(ran.toList, List(1))
    assert(clock.onPong(b))
    assertEquals(ran.toList, List(1, 2))

  test("combat needs a hit: a close target without attacks is never combat"):
    val h = Harness()
    for _ <- 0 until 150 do h.tick()
    h.spawnTarget(2, 2.5)
    for _ <- 0 until 100 do h.tick()
    val hist = h.sim.history
    assert((hist.oldestSeq until hist.nextSeq).forall(seq => hist(Schema.Gate, seq) == 0f))
    assert((hist.oldestSeq until hist.nextSeq).forall(seq => hist(Schema.MaskTgt, seq) == 0f))

  test("combat starts at a hit and lasts combatTicks after the last hit"):
    val cfg = SimConfig(calmMinTicks = 40, calmMaxTicks = 200)
    val h = Harness(cfg)
    for _ <- 0 until 150 do h.tick()
    h.spawnTarget(2, 2.5)
    h.tick(attack = 2)
    val hist = h.sim.history
    val hit = hist.nextSeq - 1
    assertEquals(hist(Schema.Gate, hit), 1f)
    for _ <- 0 until cfg.combatTicks do h.tick()
    assertEquals(hist(Schema.Gate, hit + cfg.combatTicks), 1f)
    h.tick()
    assertEquals(hist(Schema.Gate, hit + cfg.combatTicks + 1), 0f)
