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

package eigen.paper.net

import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue

import com.github.retrooper.packetevents.event.PacketSendEvent
import com.github.retrooper.packetevents.protocol.ConnectionState
import com.github.retrooper.packetevents.protocol.entity.`type`.EntityType
import com.github.retrooper.packetevents.protocol.player.User
import com.github.retrooper.packetevents.wrapper.play.server.WrapperPlayServerPing

import eigen.core.entity.ClientProfile
import eigen.core.history.Window
import eigen.core.latency.TransactionClock
import eigen.core.sim.{PlayerSim, SimConfig}
import eigen.core.util.IntMap
import eigen.paper.collect.CollectSession

/** Adapter-side facts about a tracked entity that the core simulation does not need. */
final class EntityMeta(val tpe: EntityType):
  var crouching = false
  var lowPose = false
  var scale = 1f

/**
 * Everything the plugin keeps for one connection. The simulation and all fields without
 * `@volatile` belong to the connection's network thread; other threads hand work over with
 * [[post]], which runs it there before the next packet is processed.
 */
final class EigenPlayer(
    val user: User,
    val uuid: UUID,
    val handle: Int,
    val profile: ClientProfile,
    config: SimConfig,
    windows: (EigenPlayer, Window) => Unit,
):
  val clock = TransactionClock()
  val sim = PlayerSim(profile, config, w => windows(this, w))
  val entityMeta = IntMap[EntityMeta](64)

  /** Collection session in progress; set and cleared on the network thread only. */
  var session: CollectSession | Null = null

  @volatile var name: String = user.getName
  @volatile var brand: String = "unknown"
  @volatile var exempt = false
  /** True once `PLAYER_JOIN` was announced to the backend. */
  @volatile var announced = false

  // Send-path state: the last transaction each entity's movement was bracketed with.
  private val lastMove = IntMap[LastMove](64)

  private val mailbox = ConcurrentLinkedQueue[Runnable]()

  /** Runs `task` on this player's network thread before its next packet. */
  def post(task: Runnable): Unit = mailbox.add(task)

  /** Called on the network thread at the start of every received packet. */
  def drain(): Unit =
    if !mailbox.isEmpty then
      var t = mailbox.poll()
      while t != null do
        t.run()
        t = mailbox.poll()
    clock.runDue()

  /** Sends one of our transactions, if the connection is in the play state. */
  def sendTransaction(): Unit =
    if user.getEncoderState == ConnectionState.PLAY then user.sendPacket(WrapperPlayServerPing(clock.nextId().toInt))

  /** Transaction before the packet being sent; another goes out right after it. */
  def bracket(event: PacketSendEvent): Int =
    sendTransaction()
    val id = clock.lastSent
    event.getTasksAfterSend.add(() => sendTransaction())
    id

  /**
   * Transaction that orders a movement of `entityId`. Two moves of one entity must not share a
   * transaction, or the second would be applied before the first settled.
   */
  def movementTransaction(entityId: Int): Int =
    val last = lastMove.getOrNull(entityId)
    if last != null && last.transaction == clock.lastSent then sendTransaction()
    val id = clock.lastSent
    if last != null then last.transaction = id else lastMove(entityId) = LastMove(id)
    id

  def forgetEntity(entityId: Int): Unit = lastMove.remove(entityId)

  private final class LastMove(var transaction: Int)

  /** Bucketed transaction round-trip time, for the `ping_bucket` meta column. */
  def pingBucket: Int =
    val rtt = clock.rtt
    if rtt < 50 then 0 else if rtt < 100 then 1 else if rtt < 150 then 2 else if rtt < 250 then 3 else 4
