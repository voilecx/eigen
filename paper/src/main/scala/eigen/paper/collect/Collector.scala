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

import eigen.core.sim.WindowSystem
import eigen.core.wire.Messages
import eigen.paper.config.Settings
import eigen.paper.net.{EigenPlayer, Players}
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
  def get(uuid: UUID): CollectSession | Null = sessions.get(uuid)

  def start(target: Player, label: Label, tags: Map[String, String], note: String): Either[String, CollectSession] =
    start(target.getUniqueId, target.getName, label, tags, note, selfService = false)

  def start(
      targetUuid: UUID,
      targetName: String,
      label: Label,
      tags: Map[String, String],
      note: String,
      selfService: Boolean,
  ): Either[String, CollectSession] =
    val p = players.get(targetUuid)
    if p == null then Left(s"$targetName is not tracked yet; try again in a moment.")
    else if p.exempt then Left(s"$targetName is exempt from collection.")
    else if settings.sink.server && !stream.enabled && !settings.sink.local then Left("The backend is disabled and collect.sink is server.")
    else
      val s = CollectSession(
        id = ids.incrementAndGet(),
        uuid = UUID.randomUUID(),
        handle = p.handle,
        playerName = targetName,
        player = players.pseudonym(p.uuid),
        label = label,
        tags = tags,
        note = note,
        startedAt = System.currentTimeMillis(),
        selfService = selfService,
      )
      if sessions.putIfAbsent(targetUuid, s) != null then Left(s"$targetName is already being recorded.")
      else
        if settings.sink.server then
          stream.sessionStarted(Messages.CollectStart(s.id, s.uuid.toString, s.handle, label.wire, tags, note, s.startedAt))
        if settings.sink.local then local.start(s)
        // The session becomes visible to the simulation only after the start was queued, so no
        // window can reference a session the backend has not heard of.
        p.post(() => p.session = s)
        Right(s)

  /** Automatically starts a legit self-service session on join when `collect.auto-legit` is enabled. */
  def maybeAutoStart(p: EigenPlayer): Unit =
    if settings.autoLegit && !p.exempt && (settings.sink.local || (settings.sink.server && stream.enabled)) then
      val _ = start(p.uuid, p.name, Label.Legit, Map("source" -> "auto"), "", selfService = true)

  def stop(target: UUID): Either[String, CollectSession] = finish(target, direct = false)

  /** Stops a self-service session started via `/eigen record start` or `collect.auto-legit`. */
  def stopSelf(target: UUID): Either[String, CollectSession] =
    val s = sessions.get(target)
    if s == null then Left("You do not have an active recording.")
    else if !s.selfService then Left("Your recording was started by staff and cannot be stopped with /eigen record stop.")
    else finish(target, direct = false)

  /**
   * Ends a recording. Normally this runs on the player's network thread, after its last window;
   * when the plugin is stopping or the player is leaving, it runs on the calling thread instead,
   * because no further packets (and so no mailbox drains) will come.
   */
  private def finish(target: UUID, direct: Boolean): Either[String, CollectSession] =
    val s = sessions.remove(target)
    if s == null then Left("No recording in progress for that player.")
    else
      val stoppedAt = System.currentTimeMillis()
      val p = players.get(target)
      val close: Runnable = () =>
        if p != null then
          // Flush while the session is still attached, so the last calm segment is recorded.
          WindowSystem.flushCalm(p.sim)
          p.session = null
        if settings.sink.server then stream.sessionStopped(Messages.CollectStop(s.id, stoppedAt))
        if settings.sink.local then local.stop(s, stoppedAt)
      if p == null || direct then close.run() else p.post(close)
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
  def stopAll(): Unit = sessions.keys().asScala.toSeq.foreach(u => { val _ = finish(u, direct = true) })

  /** A player left: finish their recording now, while the player is still tracked. */
  def onQuit(uuid: UUID): Unit = { val _ = finish(uuid, direct = true) }