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
import eigen.core.sim.{Handoff, ItemKind, PlayerSim, Reason, RotationSystem, SimConfig, WindowSystem}

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

    def spawnTarget(id: Int, z: Double, kind: EntityKind = EntityKind.Player): TrackedEntity =
      val e = TrackedEntity(id, kind, TrackedEntity.LivingSteps, ClientProfile.Modern)
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
    assert(!Reason.fromRaw(w.data(reasons)(w.anchor).toInt).contains(Reason.Uncalibrated))

    val engage = h.windows.filter(_.kind == WindowKind.Engage)
    assertEquals(engage.size, 1)
    assertEquals(engage.head.engagement, 1)
    assertEquals(w.engagement, 1)

  test("hits on an NPC open combat unless NPC targets are switched off"):
    def attacks(cfg: SimConfig): Int =
      val h = Harness(cfg)
      for _ <- 0 until 150 do h.tick()
      h.spawnTarget(2, 2.5, EntityKind.Npc)
      for _ <- 0 until 30 do h.tick()
      h.tick(attack = 2)
      for _ <- 0 until cfg.engagePost do h.tick()
      h.windows.count(_.kind == WindowKind.Attack)
    val base = SimConfig(calmMinTicks = 40, calmMaxTicks = 200)
    assertEquals(attacks(base), 1)
    assertEquals(attacks(base.copy(targetNpcs = false)), 0)
    assertEquals(attacks(base.copy(targetNpcs = false, targetLiving = true)), 0)

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
    assert(!clock.onPong(0), "zero is foreign (GrimAC/other plugins), never ShardCompat's either")
    assert(!clock.onPong(-32768), "negative shorts are foreign")
    assert(!clock.onPong(70000), "ids wider than a short are not ours")
    assertEquals(clock.lastReceived, 0, "foreign pongs must not advance the shared watermark")
    assertEquals(ran.toList, List.empty[Int], "foreign pongs must not run shared-clock tasks")
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

  test("Handoff preserves mouse calibration, engagement and in-progress calm track across servers"):
    val cfg = SimConfig(calmMinTicks = 40, calmMaxTicks = 200)
    val h1 = Harness(cfg)
    h1.sim.combat.engagement = 3
    for _ <- 0 until 285 do h1.tick()
    assert(RotationSystem.calibrated(h1.sim.rotation), "source sim should be calibrated")
    assertEquals(h1.sim.calm.length, 25)

    val snapshot = Handoff.encode(h1.sim)
    val h2 = Harness(cfg)
    assert(!RotationSystem.calibrated(h2.sim.rotation))
    assert(Handoff.restore(h2.sim, snapshot))
    assert(RotationSystem.calibrated(h2.sim.rotation), "restored sim is immediately calibrated")
    assertEquals(h2.sim.combat.engagement, 3)
    assertEquals(h2.sim.calm.length, 25)

    for _ <- 0 until (cfg.bufferTicks + 15) do h2.tick()
    assertEquals(h2.sim.calm.length, 40)
    WindowSystem.flushCalm(h2.sim)
    val calm = h2.windows.filter(_.kind == WindowKind.Calm)
    assertEquals(calm.size, 1)
    assertEquals(calm.head.rows, 40)
    assertEquals(calm.head.engagement, 3)

  test("a tick closed only by CLIENT_TICK_END carries the NoMoveTick observation marker"):
    // The marker never participates in any validity mask.
    assertEquals(
      (Reason.MasksRot | Reason.MasksInp | Reason.MasksTgt) & Reason.NoMoveTick,
      Reason.None,
    )
    val h = Harness()
    h.sim.onFlying(hasPos = true, 0, 64, 0, hasRot = true, 0.5f, 0f, onGround = true)
    h.sim.onTickEnd()
    h.sim.onTickEnd()
    val hist = h.sim.history
    assert(!Reason.fromRaw(hist(Schema.Reasons, hist.nextSeq - 2).toInt).contains(Reason.NoMoveTick))
    assert(Reason.fromRaw(hist(Schema.Reasons, hist.nextSeq - 1).toInt).contains(Reason.NoMoveTick))

    // Without tick-end packets the row only exists because a movement packet closed the tick.
    val legacy = PlayerSim(ClientProfile.Modern.copy(tickEnd = false, realInput = false), SimConfig(), _ => ())
    legacy.onFlying(hasPos = true, 0, 64, 0, hasRot = true, 0.5f, 0f, onGround = true)
    legacy.onTickEnd()
    assert(!Reason.fromRaw(legacy.history(Schema.Reasons, legacy.history.nextSeq - 1).toInt).contains(Reason.NoMoveTick))

  test("rows of clients without PLAYER_INPUT carry the LegacyInput reason"):
    // Inferred inputs are an observation marker, not a validity mask: the values stay
    // usable and the model decides how to weigh them (the exact keys remain unmasked).
    assertEquals((Reason.MasksRot | Reason.MasksInp | Reason.MasksTgt) & Reason.LegacyInput, Reason.None)

    val legacy = PlayerSim(ClientProfile.Modern.copy(tickEnd = false, realInput = false), SimConfig(), _ => ())
    legacy.onFlying(hasPos = true, 0, 64, 0, hasRot = true, 0.5f, 0f, onGround = true)
    assert(Reason.fromRaw(legacy.history(Schema.Reasons, legacy.history.nextSeq - 1).toInt).contains(Reason.LegacyInput))

    val modern = PlayerSim(ClientProfile.Modern, SimConfig(), _ => ())
    modern.onFlying(hasPos = true, 0, 64, 0, hasRot = true, 0.5f, 0f, onGround = true)
    modern.onInput(true, false, false, false, false, false, false)
    modern.onTickEnd()
    assert(!Reason.fromRaw(modern.history(Schema.Reasons, modern.history.nextSeq - 1).toInt).contains(Reason.LegacyInput))

  test("the Attack reason marks attack input even without a matched target"):
    assertEquals((Reason.MasksRot | Reason.MasksInp | Reason.MasksTgt) & Reason.Attack, Reason.None)
    val h = Harness()
    h.tick(attack = 7) // no entity 7 spawned: the attack column stays 0, the input bit is set
    val hist = h.sim.history
    assertEquals(hist(Schema.Attack, hist.nextSeq - 1), 0f)
    assert(Reason.fromRaw(hist(Schema.Reasons, hist.nextSeq - 1).toInt).contains(Reason.Attack))
    h.tick()
    assert(!Reason.fromRaw(hist(Schema.Reasons, hist.nextSeq - 1).toInt).contains(Reason.Attack))
    h.spawnTarget(7, 2.5)
    h.tick(attack = 7)
    assertEquals(hist(Schema.Attack, hist.nextSeq - 1), 1f)
    assert(Reason.fromRaw(hist(Schema.Reasons, hist.nextSeq - 1).toInt).contains(Reason.Attack))

  test("ItemKind classifies namespaced item ids via suffix and exact match"):
    assertEquals(ItemKind.of("minecraft:air"), ItemKind.Empty)
    assertEquals(ItemKind.of("minecraft:netherite_sword"), ItemKind.Sword)
    assertEquals(ItemKind.of("minecraft:diamond_axe"), ItemKind.Axe)
    assertEquals(ItemKind.of("minecraft:iron_spear"), ItemKind.Spear)
    assertEquals(ItemKind.of("minecraft:mace"), ItemKind.Mace)
    assertEquals(ItemKind.of("minecraft:totem_of_undying"), ItemKind.Totem)
    assertEquals(ItemKind.of("minecraft:enchanted_golden_apple"), ItemKind.Food)
    assertEquals(ItemKind.of("minecraft:ender_pearl"), ItemKind.Other)
