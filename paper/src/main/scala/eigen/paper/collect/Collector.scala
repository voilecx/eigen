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

package eigen.paper.collect

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.CollectionConverters.*

import org.bukkit.entity.Player

import eigen.core.wire.Messages
import eigen.paper.config.Settings
import eigen.paper.net.Players
import eigen.paper.stream.StreamClient

/**
 * Dataset collection: starts and stops labelled recordings of players and records segment
 * marks. Called from commands on the server thread; changes that touch a player's simulation
 * are posted to its network thread.
 */
final class Collector(settings: Settings, players: Players, stream: StreamClient, local: LocalSink):
  private val sessions = ConcurrentHashMap[UUID, CollectSession]()
  private val ids = AtomicInteger(0)

  def active: Iterable[CollectSession] = sessions.values().asScala

  def start(target: Player, label: Label, tags: Map[String, String], note: String): Either[String, CollectSession] =
    val p = players.get(target.getUniqueId)
    if p == null then Left(s"${target.getName} is not tracked yet; try again in a moment.")
    else if settings.sink.server && !stream.enabled && !settings.sink.local then Left("The backend is disabled and collect.sink is server.")
    else
      val s = CollectSession(
        id = ids.incrementAndGet(),
        uuid = UUID.randomUUID(),
        handle = p.handle,
        playerName = target.getName,
        player = players.pseudonym(p.uuid),
        label = label,
        tags = tags,
        note = note,
        startedAt = System.currentTimeMillis(),
      )
      if sessions.putIfAbsent(target.getUniqueId, s) != null then Left(s"${target.getName} is already being recorded.")
      else
        if settings.sink.server then
          stream.sessionStarted(Messages.CollectStart(s.id, s.uuid.toString, s.handle, label.wire, tags, note, s.startedAt))
        if settings.sink.local then local.start(s)
        // The session becomes visible to the simulation only after the start was queued, so no
        // window can reference a session the backend has not heard of.
        p.post(() => p.session = s)
        Right(s)

  def stop(target: UUID): Either[String, CollectSession] =
    val s = sessions.remove(target)
    if s == null then Left("No recording in progress for that player.")
    else
      val stoppedAt = System.currentTimeMillis()
      val finish: Runnable = () =>
        if settings.sink.server then stream.sessionStopped(Messages.CollectStop(s.id, stoppedAt))
        if settings.sink.local then local.stop(s, stoppedAt)
      val p = players.get(target)
      // Detach on the network thread first, so the stop is ordered after the session's last window.
      if p != null then p.post(() => { p.session = null; p.sim.calm.flush(); finish.run() })
      else finish.run()
      Right(s)

  /** Marks a segment boundary at the player's current client tick (e.g. a cheat toggled). */
  def mark(target: UUID, on: Boolean, note: String): Either[String, CollectSession] =
    val s = sessions.get(target)
    val p = players.get(target)
    if s == null || p == null then Left("No recording in progress for that player.")
    else
      p.post: () =>
        val tick = p.sim.tick
        s.cheatActive = on
        if settings.sink.server then stream.mark(Messages.Mark(s.id, tick, if on then "on" else "off", note))
        if settings.sink.local then local.mark(s, tick, on, note)
      Right(s)

  /** Stops every recording; used when the plugin disables. */
  def stopAll(): Unit = sessions.keys().asScala.toSeq.foreach(u => { val _ = stop(u) })

  /** A player left: finish their recording. */
  def onQuit(uuid: UUID): Unit = { val _ = stop(uuid) }
