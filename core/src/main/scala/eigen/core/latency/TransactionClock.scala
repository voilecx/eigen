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

package eigen.core.latency

import java.util.ArrayDeque
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicIntegerArray

/**
 * Orders server-side knowledge by what the client has actually received.
 *
 * The server brackets interesting outbound packets with ping packets ("transactions") and the
 * client answers each with a pong in order. A task registered against transaction `n` runs once
 * the pong for `n` arrives, i.e. once the client has certainly processed everything sent before
 * it. Tasks are produced on the send path and executed on the receive path — a command buffer —
 * so all simulation state is touched from the receive side only.
 *
 * Ids are positive shorts. GrimAC uses ids `<= 0`, so both anti-cheats can run side by side, each
 * consuming only its own pongs.
 */
final class TransactionClock:
  import TransactionClock.*

  private final class Stamp(val id: Short, val sentNanos: Long)
  private final class Task(val transaction: Int, val run: Runnable)

  private val counter = AtomicInteger(0)
  // Id membership as bit sets: pending marks allocated-but-unsent ids, in-flight marks
  // sent-but-unanswered ones. Two fixed 4 KiB tables replace the per-id `java.lang.Short`
  // boxing and the per-pong stream scan this class used to pay. Ids wrap only after 32768
  // outstanding transactions, which no live connection reaches.
  private val pendingBits = AtomicIntegerArray(Bits)
  private val inFlightBits = AtomicIntegerArray(Bits)
  private val inFlight = ConcurrentLinkedQueue[Stamp]()

  /** Transactions the client has been sent / has answered. Task ids compare against these. */
  private val sent = AtomicInteger(0)
  private val received = AtomicInteger(0)

  private val tasks = ArrayDeque[Task]()

  @volatile private var rttMillis = 0.0

  def lastSent: Int = sent.get
  def lastReceived: Int = received.get

  /** Smoothed round-trip time of transactions, in milliseconds (0 until the first answer). */
  def rtt: Double = rttMillis

  /** Allocates the id for the next ping we send. The caller writes the packet. */
  def nextId(): Short =
    val raw = counter.getAndIncrement() & 0x7fff
    val id = (if raw == 0 then 0x7fff else raw).toShort
    setBit(pendingBits, id)
    id

  /** Called on the send path when a ping leaves; ignores pings that are not ours. */
  def onPingSent(id: Int): Unit =
    if isOurs(id) then
      val sid = id.toShort
      if testAndClearBit(pendingBits, sid) then
        setBit(inFlightBits, sid)
        inFlight.add(Stamp(sid, System.nanoTime()))
        sent.incrementAndGet()

  /**
   * Called on the receive path for every pong. Returns true when the pong was ours, so the
   * caller can cancel it; due tasks run immediately on the calling thread.
   */
  def onPong(id: Int): Boolean =
    if !isOurs(id) || !testBit(inFlightBits, id.toShort) then return false
    var stamp: Stamp | Null = null
    while
      stamp = inFlight.poll()
      stamp != null && { received.incrementAndGet(); clearBit(inFlightBits, stamp.nn.id); stamp.nn.id != id.toShort }
    do ()
    val s = stamp
    if s != null then
      val sample = (System.nanoTime() - s.sentNanos) / 1e6
      rttMillis = if rttMillis == 0 then sample else rttMillis + (sample - rttMillis) * RttWeight
    runDue()
    true

  /** Schedules `task` for when the client has answered transaction `transaction`. */
  def at(transaction: Int)(task: Runnable): Unit =
    tasks.synchronized(tasks.addLast(Task(transaction, task)))

  /** Runs tasks whose transaction is already answered (receive path only). */
  def runDue(): Unit =
    val done = received.get
    while
      val next = tasks.synchronized {
        val head = tasks.peekFirst()
        if head != null && head.transaction <= done then tasks.pollFirst() else null
      }
      next != null && { next.run.run(); true }
    do ()

  def clear(): Unit =
    tasks.synchronized(tasks.clear())

object TransactionClock:
  private inline val RttWeight = 0.125

  /** One bit per positive short id: 32768 ids in 1024 ints. */
  private inline val Bits = 1024

  /** Ours are positive shorts; GrimAC's are `<= 0`; anything wider than a short belongs to neither. */
  inline def isOurs(id: Int): Boolean = id > 0 && id == id.toShort

  // Bit twiddles as CAS loops: contention is one writer per id, so they run uncontended
  // and allocate nothing.
  private def setBit(bits: AtomicIntegerArray, id: Short): Unit =
    val b = id.toInt & 0x7fff
    val w = b >>> 5
    val m = 1 << (b & 31)
    var prev = bits.get(w)
    while (prev & m) == 0 && !bits.compareAndSet(w, prev, prev | m) do prev = bits.get(w)

  private def clearBit(bits: AtomicIntegerArray, id: Short): Unit =
    val b = id.toInt & 0x7fff
    val w = b >>> 5
    val m = 1 << (b & 31)
    var prev = bits.get(w)
    while (prev & m) != 0 && !bits.compareAndSet(w, prev, prev & ~m) do prev = bits.get(w)

  private def testBit(bits: AtomicIntegerArray, id: Short): Boolean =
    val b = id.toInt & 0x7fff
    (bits.get(b >>> 5) & (1 << (b & 31))) != 0

  private def testAndClearBit(bits: AtomicIntegerArray, id: Short): Boolean =
    val b = id.toInt & 0x7fff
    val w = b >>> 5
    val m = 1 << (b & 31)
    var prev = bits.get(w)
    while (prev & m) != 0 && !bits.compareAndSet(w, prev, prev & ~m) do prev = bits.get(w)
    (prev & m) != 0
