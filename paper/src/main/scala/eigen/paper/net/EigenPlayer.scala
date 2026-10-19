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

import eigen.core.entity.ClientProfile
import eigen.core.history.Window
import eigen.core.latency.TransactionClock
import eigen.core.sim.{PlayerSim, SimConfig}
import eigen.core.util.IntMap
import eigen.paper.collect.CollectSession
import eigen.shard.ShardTransactionSource

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
    val pseudonym: String,
    config: SimConfig,
    windows: (EigenPlayer, Window) => Unit,
):
  val clock = TransactionClock()
  val sim = PlayerSim(profile, config, w => windows(this, w))

  /**
   * ShardCompat's view of this player's clock: the single transaction source for both.
   * Sends go through [[sendTransaction]]/[[bracket]] so `lastSent` advances synchronously
   * via the outbound listener, exactly as for Eigen's own brackets.
   */
  // Lazy so the disabled-by-default ShardCompat costs nothing per player until bound.
  lazy val shardTransactions: ShardTransactionSource = new ShardTransactionSource:
    def lastSent: Int = clock.lastSent
    def lastReceived: Int = clock.lastReceived
    def rttMillis: Double = clock.rtt
    def sendTransaction(): Unit = EigenPlayer.this.sendTransaction()
    override def sendAfter(event: PacketSendEvent): Unit = EigenPlayer.this.sendAfter(event)
    override def preTransaction(event: PacketSendEvent): Int = EigenPlayer.this.preTransaction(event)
    def bracket(event: PacketSendEvent): Int = EigenPlayer.this.bracket(event)
    def schedule(transaction: Int, task: Runnable): Unit = clock.at(transaction)(task)
  val entityMeta = IntMap[EntityMeta](64)

  /** Collection session in progress; set and cleared on the network thread only. */
  var session: CollectSession | Null = null

  @volatile var name: String = user.getName
  @volatile var brand: String = "unknown"
  @volatile var exempt = false
  /** True once `PLAYER_JOIN` was announced to the backend. */
  @volatile var announced = false

  // Send-path state: the last transaction each entity's movement/despawn was ordered with, and
  // the last outbound event already bracketed so Eigen and ShardCompat share one ping pair per packet.
  private val lastMove = IntMap[LastMove](64)
  private val despawned = IntMap[java.lang.Boolean](16)
  private var despawnedAtSent: Int = 0
  private var lastPreEvent: PacketSendEvent | Null = null
  private var lastPreId: Int = 0
  private var lastBracketEvent: PacketSendEvent | Null = null
  private var lastBracketId: Int = 0
  private var lastAfterEvent: PacketSendEvent | Null = null

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

  /**
   * Sends one of our transactions, if the connection is in the play state.
   * Dispatches through [[ServerProtocol.current]] (`PING` on 1.17+, window-0 `WINDOW_CONFIRMATION`
   * on `< 1.17`) without per-call version branching.
   */
  def sendTransaction(): Unit =
    if user.getEncoderState == ConnectionState.PLAY then
      ServerProtocol.current.sendTransaction(user, clock.nextId())

  /**
   * Schedules a single trailing transaction after `event` is sent (e.g. `JOIN_GAME`).
   * Idempotent for a given `PacketSendEvent` across Eigen and ShardCompat.
   */
  def sendAfter(event: PacketSendEvent): Unit =
    if event eq null then sendTransaction()
    else if !(event eq lastAfterEvent) then
      lastAfterEvent = event
      event.getTasksAfterSend.add(() => sendTransaction())

  /**
   * Transaction before `event` (without a trailing transaction). Shared across listeners firing
   * on the same `PacketSendEvent` so Eigen and ShardCompat send only one ping.
   */
  def preTransaction(event: PacketSendEvent): Int =
    if (event ne null) && (event eq lastPreEvent) then lastPreId
    else
      sendTransaction()
      val id = clock.lastSent
      if event ne null then
        lastPreEvent = event
        lastPreId = id
      id

  /**
   * Transaction before the packet being sent; another goes out right after it.
   * Idempotent for a given `PacketSendEvent` so Eigen and ShardCompat share the same bracket.
   */
  def bracket(event: PacketSendEvent): Int =
    if (event ne null) && (event eq lastBracketEvent) then lastBracketId
    else
      sendTransaction()
      val id = clock.lastSent
      if event ne null then
        lastBracketEvent = event
        lastBracketId = id
        sendAfter(event)
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

  /**
   * Transaction that orders a spawn of `entityId`: if `entityId` was despawned at the current
   * `lastSent`, forces a fresh transaction so `destroyTrans + 1` (which removes the old entity)
   * does not erase the newly spawned entity.
   */
  def spawnTransaction(entityId: Int): Int =
    lastMove.remove(entityId)
    if despawned.getOrNull(entityId) != null then
      if clock.lastSent <= despawnedAtSent then sendTransaction()
      despawned.clear()
    clock.lastSent

  /** Marks `entityId` despawned at the current `lastSent` and clears its movement state. */
  def noteDespawned(entityId: Int): Unit =
    lastMove.remove(entityId)
    despawned(entityId) = java.lang.Boolean.TRUE
    despawnedAtSent = clock.lastSent

  private final class LastMove(var transaction: Int)

  /** Bucketed transaction round-trip time, for the `ping_bucket` meta column. */
  def pingBucket: Int =
    val rtt = clock.rtt
    if rtt < 50 then 0 else if rtt < 100 then 1 else if rtt < 150 then 2 else if rtt < 250 then 3 else 4
