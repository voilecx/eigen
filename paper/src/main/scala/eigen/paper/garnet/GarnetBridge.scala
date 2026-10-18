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

package eigen.paper.garnet

import java.time.Duration
import java.util.Base64
import java.util.concurrent.ConcurrentHashMap
import java.util.logging.{Level, Logger}

import scala.jdk.CollectionConverters.*

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import io.lettuce.core.{ClientOptions, RedisClient, RedisURI}
import io.lettuce.core.api.StatefulRedisConnection
import io.lettuce.core.pubsub.{RedisPubSubAdapter, StatefulRedisPubSubConnection}

import eigen.paper.config.{GarnetSettings, Settings}

final case class ViolationStatus(
    player: String,
    vl: Long,
    label: String,
    score: Double,
    model: String,
    server: String,
    updatedAt: Long,
)

final case class AlertBroadcast(
    server: String,
    player: String,
    label: String,
    score: Double,
    model: String,
    vl: Long,
    ts: Long,
)

object AlertBroadcast:
  given JsonValueCodec[AlertBroadcast] = JsonCodecMaker.make

/**
 * Microsoft Garnet (Redis RESP) integration for multi-instance deployments:
 *   - Cross-server player state handoff (`eigen:handoff:<pseudonym>`) preserving mouse sensitivity
 *     calibration, engagement counter, and in-progress calm track across lobby/arena switches.
 *   - Shared violation counters (`eigen:vl:<player_lower>`) and cross-server staff alert Pub/Sub
 *     (`eigen:alerts`).
 *
 * Falls back transparently to local in-memory state when `garnet.enabled` is `false`.
 */
final class GarnetBridge(settings: Settings, log: Logger):
  private val cfg: GarnetSettings = settings.garnet
  private val localVl = ConcurrentHashMap[String, ViolationStatus]()

  @volatile private var client: RedisClient | Null = null
  @volatile private var conn: StatefulRedisConnection[String, String] | Null = null
  @volatile private var pubSub: StatefulRedisPubSubConnection[String, String] | Null = null

  private val ChannelAlerts = "eigen:alerts"
  private def handoffKey(pseudonym: String): String = s"eigen:handoff:$pseudonym"
  private def vlKey(playerName: String): String = s"eigen:vl:${playerName.toLowerCase}"

  def enabled: Boolean = cfg.enabled
  def connected: Boolean =
    val c = conn
    c != null && c.isOpen

  def start(onRemoteAlert: AlertBroadcast => Unit): Unit =
    if !cfg.enabled then return
    Thread
      .ofPlatform()
      .daemon(true)
      .name("eigen-garnet-init")
      .start: () =>
        try
          val uri = RedisURI.create(cfg.uri)
          uri.setTimeout(Duration.ofSeconds(5))
          val rc = RedisClient.create(uri)
          rc.setOptions(ClientOptions.builder().autoReconnect(true).build())
          val mainConn = rc.connect()
          val subConn = rc.connectPubSub()
          subConn.addListener(
            new RedisPubSubAdapter[String, String]:
              override def message(channel: String, message: String): Unit =
                if channel == ChannelAlerts then
                  try
                    val alert = readFromString[AlertBroadcast](message)
                    if alert.server != cfg.server then onRemoteAlert(alert)
                  catch case e: Exception => log.log(Level.FINE, "Malformed alert on eigen:alerts", e)
          )
          subConn.async().subscribe(ChannelAlerts)
          client = rc
          conn = mainConn
          pubSub = subConn
          log.info(s"Garnet connected (${cfg.uri}, server=${cfg.server})")
        catch
          case e: Exception =>
            log.log(Level.WARNING, s"Could not connect to Garnet at ${cfg.uri}; running in local-only mode", e)

  def stop(): Unit =
    try
      val ps = pubSub
      pubSub = null
      if ps != null then ps.close()
    catch case _: Exception => ()
    try
      val c = conn
      conn = null
      if c != null then c.close()
    catch case _: Exception => ()
    try
      val rc = client
      client = null
      if rc != null then rc.shutdown(Duration.ZERO, Duration.ofSeconds(2))
    catch case _: Exception => ()

  /** Saves a player's serialized handoff snapshot asynchronously with the configured TTL. */
  def saveHandoff(pseudonym: String, payload: Array[Byte]): Unit =
    val c = conn
    if c != null && c.isOpen then
      val encoded = Base64.getEncoder.encodeToString(payload)
      c.async()
        .setex(handoffKey(pseudonym), cfg.ttlSeconds.toLong, encoded)
        .exceptionally: err =>
          log.log(Level.FINE, "Failed to save handoff to Garnet", err)
          null

  /** Fetches a player's serialized handoff snapshot asynchronously, if present. */
  def loadHandoff(pseudonym: String)(onLoaded: Array[Byte] => Unit): Unit =
    val c = conn
    if c != null && c.isOpen then
      c.async()
        .get(handoffKey(pseudonym))
        .thenAccept: encoded =>
          if encoded != null && encoded.nonEmpty then
            try
              val bytes = Base64.getDecoder.decode(encoded)
              onLoaded(bytes)
            catch case _: IllegalArgumentException => ()
        .exceptionally: err =>
          log.log(Level.FINE, "Failed to load handoff from Garnet", err)
          null

  /** Increments a player's violation count, publishes a cross-server alert, and returns the updated VL. */
  def recordViolation(playerName: String, label: String, score: Double, model: String)(onRecorded: ViolationStatus => Unit): Unit =
    val now = System.currentTimeMillis()
    val key = playerName.toLowerCase
    val localUpdated = localVl.compute(
      key,
      (_, prev) =>
        val nextVl = if prev == null then 1L else prev.vl + 1L
        ViolationStatus(playerName, nextVl, label, score, model, cfg.server, now),
    )
    val c = conn
    if c == null || !c.isOpen then onRecorded(localUpdated)
    else
      val rKey = vlKey(playerName)
      val async = c.async()
      val fields = Map(
        "player" -> playerName,
        "label" -> label,
        "score" -> score.toString,
        "model" -> model,
        "server" -> cfg.server,
        "updatedAt" -> now.toString,
      ).asJava
      async
        .hincrby(rKey, "vl", 1L)
        .thenCompose: vl =>
          val status = ViolationStatus(playerName, vl.longValue(), label, score, model, cfg.server, now)
          localVl.put(key, status)
          val _ = async.hset(rKey, fields)
          val _ = async.expire(rKey, cfg.ttlSeconds.toLong)
          val payload = writeToString(AlertBroadcast(cfg.server, playerName, label, score, model, status.vl, now))
          async.publish(ChannelAlerts, payload).thenApply(_ => status)
        .whenComplete: (status, err) =>
          if err != null || status == null then
            log.log(Level.FINE, "Failed to record violation in Garnet", err)
            onRecorded(localUpdated)
          else onRecorded(status)

  /** Looks up the latest violation status for `playerName` from Garnet or local state. */
  def queryViolation(playerName: String)(onResult: Option[ViolationStatus] => Unit): Unit =
    val key = playerName.toLowerCase
    val fallback = Option(localVl.get(key))
    val c = conn
    if c == null || !c.isOpen then onResult(fallback)
    else
      c.async()
        .hgetall(vlKey(playerName))
        .whenComplete: (map, err) =>
          if err != null || map == null || map.isEmpty then onResult(fallback)
          else
            val m = map.asScala
            val parsed =
              for
                vlStr <- m.get("vl")
                vl <- vlStr.toLongOption
                label <- m.get("label")
                scoreStr <- m.get("score")
                score <- scoreStr.toDoubleOption
              yield ViolationStatus(
                player = m.getOrElse("player", playerName),
                vl = vl,
                label = label,
                score = score,
                model = m.getOrElse("model", "unknown"),
                server = m.getOrElse("server", cfg.server),
                updatedAt = m.get("updatedAt").flatMap(_.toLongOption).getOrElse(0L),
              )
            onResult(parsed.orElse(fallback))
