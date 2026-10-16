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

package eigen.paper.stream

import java.io.ByteArrayOutputStream
import java.net.http.{HttpClient, WebSocket}
import java.nio.ByteBuffer
import java.security.SecureRandom
import java.time.Duration
import java.util.ArrayDeque
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.concurrent.{CompletionStage, ConcurrentHashMap, ConcurrentLinkedQueue, Executors, ScheduledExecutorService, TimeUnit}
import java.util.logging.{Level, Logger}

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

import eigen.core.history.Window
import eigen.core.wire.{ByteSink, FrameCodec, MessageType, Messages}
import eigen.paper.config.BackendSettings

/** Connection state, for status output. */
enum StreamState:
  case Disabled, Disconnected, Connecting, Handshaking, Ready

/**
 * The plugin's side of the backend stream (protocol v1).
 *
 * Producers on any thread hand messages to a lock-free inbox. A single "eigen-stream" thread
 * owns everything else: it turns the inbox into numbered frames, keeps them until the server
 * acknowledges them, (re)connects with backoff and resends what the server has not applied.
 */
final class StreamClient(settings: BackendSettings, plugin: String, platform: String, log: Logger):
  import StreamClient.*

  private sealed trait Out
  private final case class Control(tpe: Int, body: Array[Byte]) extends Out
  private final case class Win(w: Window) extends Out
  private final class Frame(val seq: Long, val bytes: Array[Byte], val windows: Boolean)

  private val exec: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(r =>
    val t = Thread(r, "eigen-stream")
    t.setDaemon(true)
    t,
  )
  private lazy val http = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build()

  private val inbox = ConcurrentLinkedQueue[Out]()
  private val queuedWindows = AtomicInteger()
  private val droppedWindows = AtomicLong()

  // Snapshot of live state, replayed at the start of a new stream.
  private val players = ConcurrentHashMap[Int, Messages.PlayerJoin]()
  private val sessions = ConcurrentHashMap[Int, Messages.CollectStart]()

  // ---- owned by the stream thread ----
  private val codec = FrameCodec()
  private var streamId = newStreamId()
  private var nextSeq = 1L
  private var nextWindow = 1L
  private val unacked = ArrayDeque[Frame]()
  private var unackedBytes = 0L
  private var transmittedThrough = 0L
  private var socket: WebSocket | Null = null
  private var maxFrameBytes = 4 * 1024 * 1024
  private var backoffMs = MinBackoffMs
  private var reconnectAt = 0L

  @volatile private var unackedNow = 0
  @volatile private var stateNow: StreamState = if settings.enabled then StreamState.Disconnected else StreamState.Disabled
  @volatile private var inferenceNow = false
  @volatile var onVerdict: Messages.Verdict => Unit = _ => ()

  def state: StreamState = stateNow
  def enabled: Boolean = settings.enabled
  /** The server runs a model; windows should carry the `infer` purpose. */
  def inference: Boolean = inferenceNow
  def dropped: Long = droppedWindows.get
  def backlog: Int = queuedWindows.get
  /** Frames sent or waiting that the backend has not acknowledged yet. */
  def unacknowledged: Int = unackedNow

  def start(): Unit =
    if settings.enabled then
      if settings.token.isEmpty then log.warning("backend.token is empty; the backend will refuse the stream")
      exec.scheduleWithFixedDelay(() => guarded(pump()), PumpMs, PumpMs, TimeUnit.MILLISECONDS)

  def stop(): Unit =
    exec.execute: () =>
      val s = socket
      if s != null then s.sendClose(WebSocket.NORMAL_CLOSURE, "shutdown")
      socket = null
    exec.shutdown()
    exec.awaitTermination(5, TimeUnit.SECONDS)

  // ---- producer API, any thread ----

  def playerJoined(m: Messages.PlayerJoin): Unit =
    players.put(m.handle, m)
    control(MessageType.PlayerJoin, m)

  def playerLeft(handle: Int): Unit =
    players.remove(handle)
    control(MessageType.PlayerLeave, Messages.PlayerLeave(handle))

  def sessionStarted(m: Messages.CollectStart): Unit =
    sessions.put(m.session, m)
    control(MessageType.CollectStart, m)

  def sessionStopped(m: Messages.CollectStop): Unit =
    sessions.remove(m.session)
    control(MessageType.CollectStop, m)

  def mark(m: Messages.Mark): Unit = control(MessageType.Mark, m)

  /** Queues a window; drops it when the backlog is full rather than growing without bound. */
  def window(w: Window): Unit =
    if !settings.enabled then return
    if queuedWindows.incrementAndGet() > MaxQueuedWindows then
      queuedWindows.decrementAndGet()
      droppedWindows.incrementAndGet()
    else inbox.add(Win(w))

  private def control[A: JsonValueCodec](tpe: Int, m: A): Unit =
    if settings.enabled then inbox.add(Control(tpe, Messages.encode(m)))

  // ---- stream thread ----

  private def pump(): Unit =
    frameInbox()
    if unackedBytes > settings.resendBufferBytes then resetStream("resend buffer overflow")
    stateNow match
      case StreamState.Ready => transmit()
      case StreamState.Disconnected if System.currentTimeMillis() >= reconnectAt => connect()
      case _ => ()

  /** Turns queued messages into numbered frames, batching windows between control messages. */
  private def frameInbox(): Unit =
    val batch = ArrayBuffer[Window]()
    var batchBytes = 0
    def flushBatch(): Unit =
      if batch.nonEmpty then
        batch.foreach: w =>
          w.id = (nextWindow & 0xffffffffL).toInt
          nextWindow += 1
        add(codec.encodeWindows(nextSeq, batch), windows = true)
        batch.clear()
        batchBytes = 0
    var out = inbox.poll()
    while out != null do
      out match
        case Control(tpe, body) =>
          flushBatch()
          add(codec.encode(tpe, nextSeq, body), windows = false)
        case Win(w) =>
          queuedWindows.decrementAndGet()
          if batchBytes + w.wireBytes > BatchBytes then flushBatch()
          batch += w
          batchBytes += w.wireBytes
      out = inbox.poll()
    flushBatch()

  private def add(bytes: Array[Byte], windows: Boolean): Unit =
    unacked.addLast(Frame(nextSeq, bytes, windows))
    unackedBytes += bytes.length
    unackedNow = unacked.size
    nextSeq += 1

  private def transmit(): Unit =
    val s = socket
    if s == null then return
    val it = unacked.iterator()
    while it.hasNext do
      val f = it.next()
      if f.seq > transmittedThrough then
        if !send(s, f.bytes) then return
        transmittedThrough = f.seq

  private def send(s: WebSocket, bytes: Array[Byte]): Boolean =
    try
      s.sendBinary(ByteBuffer.wrap(bytes), true).get(SendTimeoutSec, TimeUnit.SECONDS)
      true
    catch
      case e: Exception =>
        disconnected(s"send failed: ${rootMessage(e)}")
        false

  private def connect(): Unit =
    stateNow = StreamState.Connecting
    val listener = Listener()
    http
      .newWebSocketBuilder()
      .header("Authorization", s"Bearer ${settings.token}")
      .header("Eigen-Protocol", "1")
      .connectTimeout(Duration.ofSeconds(10))
      .buildAsync(settings.url, listener)
      .whenComplete: (ws, error) =>
        exec.execute: () =>
          if error != null then disconnected(s"connect failed: ${rootMessage(error)}")
          else
            socket = ws
            stateNow = StreamState.Handshaking
            val _ = send(ws, codec.encode(MessageType.Hello, 0, Messages.hello(streamId, plugin, platform)))

  private def onMessage(bytes: Array[Byte]): Unit =
    FrameCodec.decode(bytes, maxFrameBytes) match
      case Left(problem) => disconnected(s"bad frame from server: $problem")
      case Right(frame) =>
        frame.tpe match
          case MessageType.Welcome => Messages.decode[Messages.Welcome](frame.payload).fold(p => disconnected(s"bad WELCOME: $p"), welcome)
          case MessageType.Ack if frame.payload.length >= 4 => acknowledge(ByteSink.readU32(frame.payload, 0))
          case MessageType.Verdict =>
            Messages.decode[Messages.Verdict](frame.payload).foreach(v => guarded(onVerdict(v)))
          case MessageType.Error =>
            Messages.decode[Messages.ServerError](frame.payload).foreach: e =>
              log.warning(s"Backend error ${e.code}: ${e.message}")
              if e.code == "UNAUTHORIZED" then backoffMs = MaxBackoffMs
              if e.fatal then disconnected(s"backend closed the stream (${e.code})")
          case other => log.fine(f"Ignoring unknown message type 0x$other%02x")

  private def welcome(w: Messages.Welcome): Unit =
    if w.protocol != 1 then
      disconnected(s"backend speaks protocol ${w.protocol}")
      return
    maxFrameBytes = w.maxFrameBytes
    inferenceNow = w.inference
    if w.resumeFrom >= nextSeq then
      // The server has applied more than we ever sent on this stream: it is not ours to resume.
      resetStream("backend is ahead of this stream")
      return
    acknowledge(w.resumeFrom)
    transmittedThrough = w.resumeFrom
    stateNow = StreamState.Ready
    backoffMs = MinBackoffMs
    log.info(s"Connected to the Eigen backend (resuming after #${w.resumeFrom}, inference ${if w.inference then "on" else "off"})")
    transmit()

  private def acknowledge(seq: Long): Unit =
    while !unacked.isEmpty && unacked.peekFirst().seq <= seq do
      unackedBytes -= unacked.pollFirst().bytes.length
    unackedNow = unacked.size

  /** Starts a fresh stream, replaying live players and sessions so references stay valid. */
  private def resetStream(reason: String): Unit =
    val windows = unacked.asScala.count(_.windows)
    log.warning(s"Starting a new backend stream ($reason); dropping $windows unacknowledged window batches")
    streamId = newStreamId()
    nextSeq = 1
    unacked.clear()
    unackedBytes = 0
    unackedNow = 0
    transmittedThrough = 0
    players.values().asScala.toSeq.sortBy(_.handle).foreach(m => add(codec.encode(MessageType.PlayerJoin, nextSeq, Messages.encode(m)), windows = false))
    sessions.values().asScala.toSeq.sortBy(_.session).foreach(m => add(codec.encode(MessageType.CollectStart, nextSeq, Messages.encode(m)), windows = false))
    backoffMs = MinBackoffMs
    disconnected("new stream")

  private def disconnected(reason: String): Unit =
    val s = socket
    socket = null
    if s != null then s.abort()
    if stateNow != StreamState.Disconnected then log.warning(s"Backend stream down: $reason; retrying in ${backoffMs / 1000}s")
    stateNow = StreamState.Disconnected
    inferenceNow = false
    reconnectAt = System.currentTimeMillis() + backoffMs + (math.random() * backoffMs * 0.2).toLong
    backoffMs = math.min(MaxBackoffMs, backoffMs * 2)

  private def guarded(body: => Unit): Unit =
    try body
    catch case e: Exception => log.log(Level.SEVERE, "Backend stream task failed", e)

  /** Receives frames on the HTTP client's threads and hands complete messages to the stream thread. */
  private final class Listener extends WebSocket.Listener:
    private val buffer = ByteArrayOutputStream()

    override def onOpen(ws: WebSocket): Unit = ws.request(1)

    override def onBinary(ws: WebSocket, data: ByteBuffer, last: Boolean): CompletionStage[?] | Null =
      val chunk = new Array[Byte](data.remaining())
      data.get(chunk)
      buffer.write(chunk)
      if buffer.size() > maxFrameBytes + 64 * 1024 then exec.execute(() => disconnected("oversized frame from server"))
      else if last then
        val message = buffer.toByteArray
        buffer.reset()
        exec.execute(() => guarded(if socket == ws then onMessage(message)))
      ws.request(1)
      null

    override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] | Null =
      exec.execute(() => if socket == ws then disconnected("text frame from server"))
      null

    override def onClose(ws: WebSocket, code: Int, reason: String): CompletionStage[?] | Null =
      exec.execute(() => if socket == ws then disconnected(s"closed by backend ($code $reason)"))
      null

    override def onError(ws: WebSocket, error: Throwable): Unit =
      exec.execute(() => if socket == ws then disconnected(rootMessage(error)))

object StreamClient:
  private inline val PumpMs = 50L
  private inline val SendTimeoutSec = 10L
  private inline val MinBackoffMs = 1000L
  private inline val MaxBackoffMs = 300_000L
  private inline val BatchBytes = 512 * 1024
  private inline val MaxQueuedWindows = 20_000

  private val random = SecureRandom()

  private def newStreamId(): String = f"${random.nextLong()}%016x"

  private def rootMessage(e: Throwable): String =
    var c = e
    while c.getCause != null && c.getCause != c do c = c.getCause
    s"${c.getClass.getSimpleName}: ${c.getMessage}"
