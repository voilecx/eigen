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

package eigen.shard

import java.util.logging.Logger

import scala.collection.mutable.ArrayBuffer

import com.github.retrooper.packetevents.event.PacketSendEvent

/**
 * ShardCompat must allocate/send/consume no transaction ids of its own: everything goes
 * through the host's shared, positive-id source. The fake below mirrors the shared clock's
 * contract (positive wire ids, ordinal sent/received watermarks, tasks run on ack).
 */
class ShardTransactionBridgeSuite extends munit.FunSuite:

  private final class FakeSource extends ShardTransactionSource:
    var sent: Int = 0
    var received: Int = 0
    var rtt: Double = 0.0
    val wireIds: ArrayBuffer[Int] = ArrayBuffer.empty
    private val scheduled: ArrayBuffer[(Int, Runnable)] = ArrayBuffer.empty

    def lastSent: Int = sent
    def lastReceived: Int = received
    def rttMillis: Double = rtt

    def sendTransaction(): Unit =
      wireIds += sent + 1 // positive wire ids, in ordinal order
      sent += 1

    def bracket(event: PacketSendEvent): Int =
      sendTransaction()
      val id = sent
      sendTransaction()
      id

    def schedule(transaction: Int, task: Runnable): Unit =
      scheduled += ((transaction, task))

    /** Simulates the host clock answering transactions up to `upTo` (receive thread). */
    def ack(upTo: Int): Unit =
      received = upTo
      val (due, rest) = scheduled.partition(_._1 <= upTo)
      scheduled.clear()
      scheduled ++= rest
      due.foreach(_._2.run())

  test("bridge sends only through the shared source, with positive ids and shared ordinals"):
    val fake = FakeSource()
    val bridge = ShardTransactionBridge(() => fake)
    bridge.sendTransaction()
    assertEquals(bridge.lastSent(), 1)
    val bracketed = bridge.bracketTransaction(null.asInstanceOf[PacketSendEvent])
    assertEquals(bracketed, 2, "bracket returns the shared ordinal after its first send")
    assertEquals(bridge.lastSent(), 3, "bracket's trailing send advances the shared ordinal")
    assert(fake.wireIds.nonEmpty && fake.wireIds.forall(_ > 0), s"no negative/zero ids may be generated: ${fake.wireIds}")
    assertEquals(fake.wireIds.toList, List(1, 2, 3))

  test("shared-clock ack runs Shard latency tasks and advances the Shard watermark"):
    val fake = FakeSource()
    val bridge = ShardTransactionBridge(() => fake)
    val utils = ShardLatencyUtils(bridge, "test", Logger.getLogger("test"))
    val ran = ArrayBuffer.empty[Int]
    bridge.sendTransaction()
    utils.addRealTimeTask(bridge.lastSent(), () => ran += 1)
    bridge.sendTransaction()
    utils.addRealTimeTask(bridge.lastSent(), () => ran += 2)
    assertEquals(ran.toList, List.empty[Int], "tasks must not run before the shared ack")
    fake.ack(1)
    assertEquals(bridge.lastReceived(), 1)
    assertEquals(ran.toList, List(1))
    fake.ack(2)
    assertEquals(bridge.lastReceived(), 2)
    assertEquals(ran.toList, List(1, 2))

  test("tasks queued before the host is bound are transferred to the shared clock in order"):
    var bound: ShardTransactionSource | Null = null
    val bridge = ShardTransactionBridge(() => bound)
    val utils = ShardLatencyUtils(bridge, "test", Logger.getLogger("test"))
    val ran = ArrayBuffer.empty[Int]
    utils.addRealTimeTask(1, () => ran += 1)
    val fake = FakeSource()
    fake.sendTransaction()
    bound = fake
    utils.flushPending()
    assertEquals(ran.toList, List.empty[Int])
    fake.ack(1)
    assertEquals(ran.toList, List(1))

  test("playerPing is maintained from the shared RTT"):
    // `ShardPlayer.syncPlayerPing` copies exactly this value into `tracking.playerPing`;
    // a full ShardPlayer cannot be built offline (PacketEvents registries need a server).
    val fake = FakeSource()
    val bridge = ShardTransactionBridge(() => fake)
    fake.rtt = 87.6
    assertEquals(bridge.playerPingMillis, 88)
    fake.rtt = 20.2
    assertEquals(bridge.playerPingMillis, 20)
