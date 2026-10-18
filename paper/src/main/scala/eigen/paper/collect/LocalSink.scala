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

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import java.util.logging.{Level, Logger}

import scala.collection.mutable

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import eigen.core.history.Window
import eigen.core.schema.Manifest
import eigen.core.wire.FrameCodec

/**
 * Writes collected sessions to disk, in the layout the backend stores them:
 *
 * {{{
 * collect/<label>_<player>_<utc time>_<id>/
 *   manifest.json   schema manifest (protocol §4)
 *   session.json    session metadata, rewritten when the session stops
 *   windows.bin     records of: u32 length, then one window exactly as in WINDOWS (§5)
 *   marks.jsonl     segment boundaries recorded with `collect mark`
 * }}}
 *
 * All file work happens on one background thread; the network thread only enqueues.
 */
final class LocalSink(root: Path, log: Logger):
  import LocalSink.*

  private val io: ExecutorService = Executors.newSingleThreadExecutor(r =>
    val t = Thread(r, "eigen-collect-io")
    t.setDaemon(true)
    t,
  )
  private val codec = FrameCodec()
  private val open = mutable.HashMap[Int, Open]()

  private final class Open(val dir: Path, val windows: FileChannel):
    var nextId = 1

  /** Computes the local session directory path for `s`. */
  def sessionDir(s: CollectSession): Path =
    val time = Stamp.format(Instant.ofEpochMilli(s.startedAt))
    root.resolve(s"${s.label.wire}_${safe(s.playerName)}_${time}_${s.uuid.toString.take(8)}")

  def start(s: CollectSession): Unit = io.execute: () =>
    attempt(s, "open"):
      val dir = sessionDir(s)
      Files.createDirectories(dir)
      Files.write(dir.resolve("manifest.json"), Manifest.bytes)
      writeMeta(dir, s, stoppedAt = 0L)
      val channel = FileChannel.open(dir.resolve("windows.bin"), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
      open(s.id) = Open(dir, channel)

  def write(s: CollectSession, w: Window): Unit = io.execute: () =>
    open.get(s.id).foreach: o =>
      attempt(s, "write"):
        val record = codec.windowRecord(w, Window.PurposeCollect, o.nextId)
        o.nextId += 1
        val len = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(record.length).flip()
        o.windows.write(Array(len, ByteBuffer.wrap(record)))

  def mark(s: CollectSession, tick: Int, on: Boolean, note: String): Unit = io.execute: () =>
    open.get(s.id).foreach: o =>
      attempt(s, "mark"):
        val line = writeToString(MarkLine(tick, if on then "on" else "off", note, System.currentTimeMillis())) + "\n"
        Files.write(o.dir.resolve("marks.jsonl"), line.getBytes(UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND)

  def stop(s: CollectSession, stoppedAt: Long): Unit = io.execute: () =>
    open.remove(s.id).foreach: o =>
      attempt(s, "close"):
        o.windows.force(true)
        o.windows.close()
        writeMeta(o.dir, s, stoppedAt)

  /** Finishes queued writes; used on plugin disable. */
  def shutdown(): Unit =
    io.shutdown()
    if !io.awaitTermination(10, TimeUnit.SECONDS) then log.warning("Collection writes did not finish in time")

  private def writeMeta(dir: Path, s: CollectSession, stoppedAt: Long): Unit =
    val meta = SessionMeta(
      sessionUuid = s.uuid.toString,
      player = s.player,
      playerName = s.playerName,
      label = s.label.wire,
      tags = s.tags,
      note = s.note,
      startedAtMs = s.startedAt,
      stoppedAtMs = stoppedAt,
      windows = s.windows.get,
      schemaHash = Manifest.hash,
    )
    val tmp = dir.resolve("session.json.tmp")
    Files.write(tmp, writeToArray(meta, WriterConfig.withIndentionStep(2)))
    Files.move(tmp, dir.resolve("session.json"), java.nio.file.StandardCopyOption.REPLACE_EXISTING, java.nio.file.StandardCopyOption.ATOMIC_MOVE)

  private inline def attempt(s: CollectSession, what: String)(body: => Unit): Unit =
    try body
    catch case e: java.io.IOException => log.log(Level.SEVERE, s"Collection session ${s.uuid}: $what failed", e)

object LocalSink:
  private val Stamp = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss").withZone(ZoneOffset.UTC)

  private def safe(s: String): String = s.replaceAll("[^A-Za-z0-9_-]", "-")

  private final case class SessionMeta(
      sessionUuid: String,
      player: String,
      playerName: String,
      label: String,
      tags: Map[String, String],
      note: String,
      startedAtMs: Long,
      stoppedAtMs: Long,
      windows: Int,
      schemaHash: String,
  )
  private final case class MarkLine(tick: Int, state: String, note: String, atMs: Long)

  private given JsonValueCodec[SessionMeta] =
    JsonCodecMaker.make(CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case).withTransientEmpty(false))
  private given JsonValueCodec[MarkLine] =
    JsonCodecMaker.make(CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case))
