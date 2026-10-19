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

package eigen.paper.config

import java.net.URI
import java.security.SecureRandom
import java.util.HexFormat

import org.bukkit.plugin.java.JavaPlugin

import eigen.config.Read
import eigen.core.sim.SimConfig
import eigen.shard.ShardCompatConfig

enum CollectSink:
  case Server, Local, Both

  def server: Boolean = this != Local
  def local: Boolean = this != Server

final case class BackendSettings(enabled: Boolean, url: URI, token: String, resendBufferBytes: Long)

final case class GarnetSettings(enabled: Boolean, uri: URI, server: String, ttlSeconds: Int)

final case class Settings(
    backend: BackendSettings,
    salt: Array[Byte],
    sink: CollectSink,
    autoLegit: Boolean,
    shardCompat: ShardCompatConfig,
    garnet: GarnetSettings,
    sim: SimConfig,
    exemptBedrock: Boolean,
    alertThreshold: Double,
)

/**
 * The plugin's configuration. Every key is declared once in [[Settings.schema]] with its type,
 * default and check; the bundled `config.yml` documents the same keys and must read back to the
 * same defaults (see the settings test).
 */
object Settings:

  private val defaults = SimConfig()
  private val DefaultEndpoint = URI.create("wss://eigen.example.com/v1/stream")
  private val DefaultGarnetUri = URI.create("redis://127.0.0.1:6379")
  private val LoopbackHosts = Set("localhost", "127.0.0.1", "[::1]", "::1")
  private val SaltHexDigits = 64

  /** Reads `config.yml` from the plugin, writing defaults and a fresh privacy salt on first start. */
  def load(plugin: JavaPlugin): Either[String, Settings] =
    plugin.saveDefaultConfig()
    plugin.reloadConfig()
    ensureSalt(plugin)
    schema.from(BukkitSource(plugin.getConfig))

  private val backendSchema: Read[BackendSettings] =
    for
      enabled <- Read.bool("enabled", default = false)
      url <- Read.key("url", DefaultEndpoint)(endpoint)
      token <- Read.text("token", "").map(_.trim)
      resendMb <- Read.int("resend-buffer-mb", default = 64, min = 1)
    yield BackendSettings(enabled, url, token, resendMb.toLong * 1024 * 1024)

  private val sinkSchema: Read[CollectSink] =
    Read.enumChoice("sink", CollectSink.Server, CollectSink.values)

  private val collectSchema: Read[(CollectSink, Boolean, ShardCompatConfig)] =
    for
      sink <- sinkSchema
      autoLegit <- Read.bool("auto-legit", default = false)
      shardCompat <- Read.bool("shard-compat", default = false)
      shardPre <- Read.int("shard-pre-window", default = 128, min = 1)
      shardPost <- Read.int("shard-post-window", default = 128, min = 1)
    yield (sink, autoLegit, ShardCompatConfig(shardCompat, shardPre, shardPost))

  private val garnetSchema: Read[GarnetSettings] =
    for
      enabled <- Read.bool("enabled", default = false)
      uri <- Read.key("uri", DefaultGarnetUri)(garnetUri)
      server <- Read.text("server", "default").map(_.trim).filterOrElse("server", "must not be empty")(_.nonEmpty)
      ttl <- Read.int("ttl-seconds", default = 600, min = 10)
    yield GarnetSettings(enabled, uri, server, ttl)

  private final case class Detection(sim: SimConfig, exemptBedrock: Boolean)

  private val detectionSchema: Read[Detection] =
    for
      living <- Read.choice("targets", "players")("players" -> false, "living" -> true)
      gate <- Read.section("gate"):
        for
          combat <- Read.int("combat-ticks", defaults.combatTicks, min = 1)
          buffer <- Read.int("buffer-ticks", defaults.bufferTicks, min = 0)
        yield (combat, buffer)
      windows <- Read.section("windows"):
        for
          attackPre <- Read.int("attack-pre", defaults.attackPre, min = 0)
          attackPost <- Read.int("attack-post", defaults.attackPost, min = 1)
          attackGap <- Read.int("attack-min-gap", defaults.attackMinGap, min = 0)
          enginePre <- Read.int("engage-pre", defaults.engagePre, min = 0)
          enginePost <- Read.int("engage-post", defaults.engagePost, min = 1)
        yield (attackPre, attackPost, attackGap, enginePre, enginePost)
      calm <- Read.section("calm"):
        for
          minTicks <- Read.int("min-ticks", defaults.calmMinTicks, min = 1)
          maxTicks <- Read.int("max-ticks", defaults.calmMaxTicks, min = 1)
          _ <- Read.ensure(maxTicks >= minTicks, "max-ticks", "must be at least min-ticks")
          minActive <- Read.double("min-active", defaults.calmMinActive.toDouble)
        yield (minTicks, maxTicks, minActive.toFloat)
      exempt <- Read.bool("exempt-bedrock", default = true)
    yield
      val (combat, buffer) = gate
      val (attackPre, attackPost, attackGap, enginePre, enginePost) = windows
      val (minTicks, maxTicks, minActive) = calm
      Detection(
        SimConfig(
          targetLiving = living,
          combatTicks = combat,
          bufferTicks = buffer,
          attackPre = attackPre,
          attackPost = attackPost,
          attackMinGap = attackGap,
          engagePre = enginePre,
          engagePost = enginePost,
          calmMinTicks = minTicks,
          calmMaxTicks = maxTicks,
          calmMinActive = minActive,
        ),
        exempt,
      )

  /** The whole configuration, assembled after its parts so their initialisation order is safe. */
  val schema: Read[Settings] =
    for
      backend <- Read.section("backend")(backendSchema)
      salt <- Read.section("privacy")(Read.key("salt", Array.empty[Byte])(hexSalt))
      collect <- Read.section("collect")(collectSchema)
      garnet <- Read.section("garnet")(garnetSchema)
      detection <- Read.section("detection")(detectionSchema)
      threshold <- Read.section("alerts")(Read.double("threshold", 0.9))
    yield
      val (sink, autoLegit, shardCompat) = collect
      Settings(backend, salt, sink, autoLegit, shardCompat, garnet, detection.sim, detection.exemptBedrock, threshold)

  /** Parses a text configuration value into a [[URI]] and runs scheme/host validation `check`. */
  private def parseUri(check: (String, URI) => Either[String, URI])(raw: Any): Either[String, URI] = raw match
    case text: String =>
      try check(text, URI(text))
      catch case e: IllegalArgumentException => Left(s"not a URI: ${e.getMessage}")
    case other => Left(s"must be text, got ${other.getClass.getSimpleName}")

  /** A Garnet/Redis endpoint: `redis://` or `rediss://`. */
  private val garnetUri: Any => Either[String, URI] = parseUri: (text, uri) =>
    uri.getScheme match
      case "redis" | "rediss" if uri.getHost != null && uri.getHost.nonEmpty => Right(uri)
      case "redis" | "rediss" => Left(s"missing host in '$text'")
      case _ => Left(s"must be redis:// or rediss://, got '$text'")

  /** A backend endpoint: `wss://` anywhere, `ws://` only for loopback development. */
  private val endpoint: Any => Either[String, URI] = parseUri: (text, uri) =>
    val loopback = uri.getHost != null && LoopbackHosts.contains(uri.getHost)
    uri.getScheme match
      case "wss" => Right(uri)
      case "ws" if loopback => Right(uri)
      case "ws" => Left("plaintext ws:// is allowed only for loopback hosts; use wss://")
      case _ => Left(s"must be ws:// or wss://, got '$text'")

  /** The privacy salt, hex-encoded. The plugin writes one on first start. */
  private def hexSalt(raw: Any): Either[String, Array[Byte]] = raw match
    case text: String if text.length >= SaltHexDigits && text.forall(Character.digit(_, 16) >= 0) =>
      Right(HexFormat.of.parseHex(text))
    case _ => Left(s"must be at least $SaltHexDigits hex digits; delete it to generate a new one")

  private def ensureSalt(plugin: JavaPlugin): Unit =
    val config = plugin.getConfig
    val salt = config.getString("privacy.salt", "")
    if salt == null || salt.length < SaltHexDigits then
      val bytes = new Array[Byte](SaltHexDigits / 2)
      SecureRandom().nextBytes(bytes)
      config.set("privacy.salt", HexFormat.of().formatHex(bytes))
      plugin.saveConfig()