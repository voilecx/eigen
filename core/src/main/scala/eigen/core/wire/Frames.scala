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

package eigen.core.wire

import java.util.zip.{DataFormatException, Deflater, Inflater}

import eigen.core.history.Window
import eigen.core.schema.ColType

/** Message types of protocol §3. */
object MessageType:
  inline val Hello = 0x01
  inline val PlayerJoin = 0x02
  inline val PlayerLeave = 0x03
  inline val CollectStart = 0x04
  inline val CollectStop = 0x05
  inline val Mark = 0x06
  inline val Windows = 0x10
  inline val Welcome = 0x81
  inline val Ack = 0x82
  inline val Verdict = 0x83
  inline val Error = 0x8f

/** A decoded server→client frame. `payload` is already decompressed. */
final case class InFrame(tpe: Int, seq: Long, payload: Array[Byte])

/**
 * Frame and payload codecs of protocol v1. One instance per transport thread: it owns reusable
 * buffers and a DEFLATE context, so encoding allocates only the finished frame.
 */
final class FrameCodec(compressAbove: Int = 1024):
  private val payload = ByteSink()
  private val frame = ByteSink()
  private val deflater = Deflater(Deflater.DEFAULT_COMPRESSION, true)
  private val chunk = new Array[Byte](64 * 1024)

  /** Encodes one frame; payloads above `compressAbove` bytes are raw-DEFLATE compressed. */
  def encode(tpe: Int, seq: Long, body: Array[Byte], len: Int): Array[Byte] =
    frame.reset()
    val compress = len > compressAbove
    frame.u8(tpe)
    frame.u8(if compress then 1 else 0)
    frame.u16(0)
    frame.u32(seq)
    if compress then
      deflater.reset()
      deflater.setInput(body, 0, len)
      deflater.finish()
      while !deflater.finished() do
        val n = deflater.deflate(chunk)
        frame.bytes(chunk, 0, n)
    else frame.bytes(body, 0, len)
    frame.toArray

  def encode(tpe: Int, seq: Long, body: Array[Byte]): Array[Byte] = encode(tpe, seq, body, body.length)

  /** Encodes a `WINDOWS` frame (protocol §5). */
  def encodeWindows(seq: Long, windows: collection.Seq[Window]): Array[Byte] =
    payload.reset()
    payload.u16(windows.length)
    windows.foreach(w => writeWindow(w, w.purpose, w.id))
    encode(MessageType.Windows, seq, payload.array, payload.size)

  /** One window exactly as it appears inside a `WINDOWS` payload, with the given purpose and id. */
  def windowRecord(w: Window, purpose: Int, id: Int): Array[Byte] =
    payload.reset()
    writeWindow(w, purpose, id)
    payload.toArray

  private def writeWindow(w: Window, purpose: Int, id: Int): Unit =
    val p = payload
    p.ensure(w.wireBytes)
    p.u32(w.handle.toLong & 0xffffffffL)
    p.u32(id.toLong & 0xffffffffL)
    p.u32(w.session.toLong & 0xffffffffL)
    p.u8(w.kind.code)
    p.u8(purpose)
    p.u8(w.layout.id)
    p.u8(0)
    p.u32(w.epoch.toLong & 0xffffffffL)
    p.u32(w.engagement.toLong & 0xffffffffL)
    p.u32(w.firstTick.toLong & 0xffffffffL)
    p.u16(w.rows)
    p.u16(w.anchor & 0xffff)
    var c = 0
    while c < w.data.length do
      val values = w.data(c)
      w.layout.cols(c).tpe match
        case ColType.F32 =>
          var i = 0
          while i < w.rows do { p.f32(values(i)); i += 1 }
        case ColType.U16 =>
          var i = 0
          while i < w.rows do { p.u16(values(i).toInt); i += 1 }
        case ColType.U8 | ColType.I8 =>
          var i = 0
          while i < w.rows do { p.u8(values(i).toInt); i += 1 }
      c += 1

object FrameCodec:
  inline val HeaderBytes = 8

  /** Parses a server frame, inflating a compressed payload up to `maxBytes`. */
  def decode(bytes: Array[Byte], maxBytes: Int): Either[String, InFrame] =
    if bytes.length < HeaderBytes then return Left("frame shorter than its header")
    val tpe = bytes(0) & 0xff
    val flags = bytes(1) & 0xff
    if (flags & ~1) != 0 || ByteSink.readU16(bytes, 2) != 0 then return Left("reserved bits set")
    val seq = ByteSink.readU32(bytes, 4)
    val body = java.util.Arrays.copyOfRange(bytes, HeaderBytes, bytes.length)
    if (flags & 1) == 0 then Right(InFrame(tpe, seq, body))
    else inflate(body, maxBytes).map(InFrame(tpe, seq, _))

  private def inflate(body: Array[Byte], maxBytes: Int): Either[String, Array[Byte]] =
    val inflater = Inflater(true)
    try
      inflater.setInput(body)
      val out = ByteSink(math.min(maxBytes, body.length * 4 + 64))
      val chunk = new Array[Byte](16 * 1024)
      while !inflater.finished() do
        val n = inflater.inflate(chunk)
        if n == 0 && (inflater.needsInput() || inflater.needsDictionary()) then return Left("truncated deflate stream")
        if out.size + n > maxBytes then return Left("frame exceeds max_frame_bytes")
        out.bytes(chunk, 0, n)
      Right(out.toArray)
    catch case e: DataFormatException => Left(s"bad deflate stream: ${e.getMessage}")
    finally inflater.end()
