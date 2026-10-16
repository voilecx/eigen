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

import org.bukkit.configuration.file.FileConfiguration
import org.bukkit.plugin.java.JavaPlugin

import eigen.core.sim.SimConfig

enum CollectSink:
  case Server, Local, Both

  def server: Boolean = this != Local
  def local: Boolean = this != Server

final case class BackendSettings(enabled: Boolean, url: URI, token: String, resendBufferBytes: Long)

final case class Settings(
    backend: BackendSettings,
    salt: Array[Byte],
    sink: CollectSink,
    sim: SimConfig,
    exemptBedrock: Boolean,
    alertThreshold: Double,
)

object Settings:

  /** Reads `config.yml`, writing defaults and a fresh privacy salt on first start. */
  def load(plugin: JavaPlugin): Either[String, Settings] =
    plugin.saveDefaultConfig()
    plugin.reloadConfig()
    val c = plugin.getConfig
    ensureSalt(plugin, c)
    for
      url <- parseUri(c.getString("backend.url", ""))
      sink <- parseSink(c.getString("collect.sink", "server"))
      targets <- parseTargets(c.getString("detection.targets", "players"))
      sim <- simConfig(c, targets)
    yield Settings(
      backend = BackendSettings(
        enabled = c.getBoolean("backend.enabled", false),
        url = url,
        token = c.getString("backend.token", "").trim,
        resendBufferBytes = math.max(1L, c.getLong("backend.resend-buffer-mb", 64)) * 1024 * 1024,
      ),
      salt = HexFormat.of().parseHex(c.getString("privacy.salt")),
      sink = sink,
      sim = sim,
      exemptBedrock = c.getBoolean("detection.exempt-bedrock", true),
      alertThreshold = c.getDouble("alerts.threshold", 0.9),
    )

  private def ensureSalt(plugin: JavaPlugin, c: FileConfiguration): Unit =
    val salt = c.getString("privacy.salt", "")
    if salt == null || salt.length < 32 then
      val bytes = new Array[Byte](32)
      SecureRandom().nextBytes(bytes)
      c.set("privacy.salt", HexFormat.of().formatHex(bytes))
      plugin.saveConfig()

  private def parseUri(s: String): Either[String, URI] =
    try
      val uri = URI(s)
      if uri.getScheme == "wss" || uri.getScheme == "ws" then Right(uri)
      else Left(s"backend.url must be ws:// or wss://, got '$s'")
    catch case e: IllegalArgumentException => Left(s"backend.url is not a URI: ${e.getMessage}")

  private def parseSink(s: String): Either[String, CollectSink] = s.toLowerCase match
    case "server" => Right(CollectSink.Server)
    case "local" => Right(CollectSink.Local)
    case "both" => Right(CollectSink.Both)
    case other => Left(s"collect.sink must be server, local or both, got '$other'")

  private def parseTargets(s: String): Either[String, Boolean] = s.toLowerCase match
    case "players" => Right(false)
    case "living" => Right(true)
    case other => Left(s"detection.targets must be players or living, got '$other'")

  private def simConfig(c: FileConfiguration, living: Boolean): Either[String, SimConfig] =
    try
      Right(
        SimConfig(
          targetLiving = living,
          gateRadius = c.getDouble("detection.gate.radius", 8.0),
          gateFov = c.getDouble("detection.gate.fov", 75.0),
          bufferTicks = c.getInt("detection.gate.buffer-ticks", 60),
          attackPre = c.getInt("detection.windows.attack-pre", 28),
          attackPost = c.getInt("detection.windows.attack-post", 12),
          attackMinGap = c.getInt("detection.windows.attack-min-gap", 12),
          engagePre = c.getInt("detection.windows.engage-pre", 12),
          engagePost = c.getInt("detection.windows.engage-post", 28),
          calmMinTicks = c.getInt("detection.calm.min-ticks", 100),
          calmMaxTicks = c.getInt("detection.calm.max-ticks", 1200),
          calmMinActive = c.getDouble("detection.calm.min-active", 0.25).toFloat,
        ),
      )
    catch case e: IllegalArgumentException => Left(s"detection: ${e.getMessage}")
