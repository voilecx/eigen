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

import java.nio.charset.StandardCharsets.UTF_8

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import eigen.core.schema.Manifest

/** JSON control messages of protocol §3, with codecs generated at compile time by jsoniter. */
object Messages:

  final case class PlayerJoin(handle: Int, player: String, clientProtocol: Int, brand: String)
  final case class PlayerLeave(handle: Int)
  final case class CollectStart(
      session: Int,
      sessionUuid: String,
      handle: Int,
      label: String,
      tags: Map[String, String],
      note: String,
      startedAtMs: Long,
  )
  final case class CollectStop(session: Int, stoppedAtMs: Long)
  final case class Mark(session: Int, tick: Int, state: String, note: String)

  final case class Welcome(protocol: Int, resumeFrom: Long, maxFrameBytes: Int, serverTimeMs: Long, inference: Boolean)
  final case class Verdict(handle: Int, window: Long, model: String, scores: Map[String, Double])
  final case class ServerError(code: String, message: String, fatal: Boolean)

  private inline def codec[A]: JsonValueCodec[A] =
    JsonCodecMaker.make[A](CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case).withTransientDefault(false).withTransientEmpty(false))

  given JsonValueCodec[PlayerJoin] = codec
  given JsonValueCodec[PlayerLeave] = codec
  given JsonValueCodec[CollectStart] = codec
  given JsonValueCodec[CollectStop] = codec
  given JsonValueCodec[Mark] = codec
  given JsonValueCodec[Welcome] = codec
  given JsonValueCodec[Verdict] = codec
  given JsonValueCodec[ServerError] = codec
  private given JsonValueCodec[String] = JsonCodecMaker.make[String]

  def encode[A: JsonValueCodec](a: A): Array[Byte] = writeToArray(a)

  def decode[A: JsonValueCodec](bytes: Array[Byte]): Either[String, A] =
    try Right(readFromArray[A](bytes))
    catch case e: JsonReaderException => Left(e.getMessage)

  /**
   * `HELLO` embeds the canonical manifest verbatim, so the server can hash exactly the bytes we
   * hashed. Built by hand because the manifest must not be re-serialised.
   */
  def hello(streamId: String, plugin: String, platform: String): Array[Byte] =
    val sb = StringBuilder(Manifest.json.length + 256)
    sb.append("{\"protocol\":1,\"stream_id\":").append(quote(streamId))
    sb.append(",\"plugin\":").append(quote(plugin))
    sb.append(",\"platform\":").append(quote(platform))
    sb.append(",\"schema\":").append(Manifest.json)
    sb.append(",\"schema_hash\":").append(quote(Manifest.hash)).append('}')
    sb.result().getBytes(UTF_8)

  private def quote(s: String): String = writeToString(s)
