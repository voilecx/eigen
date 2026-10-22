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

import java.io.InputStreamReader
import java.nio.charset.StandardCharsets.UTF_8

import com.github.retrooper.packetevents.manager.server.ServerVersion
import com.github.retrooper.packetevents.protocol.packettype.PacketType
import org.bukkit.configuration.file.YamlConfiguration

import eigen.config.Source
import eigen.paper.net.{PacketTable, ServerProtocol}

class SettingsSuite extends munit.FunSuite:

  private val salt = "ab" * 32

  /** The bundled `config.yml`, read exactly as Bukkit reads it, with a salt set as on first start. */
  private def bundled(): YamlConfiguration =
    val stream = getClass.getResourceAsStream("/config.yml")
    val config = YamlConfiguration.loadConfiguration(InputStreamReader(stream, UTF_8))
    config.set("privacy.salt", salt)
    config

  private def settingsFrom(entries: Map[String, Any]): Either[String, Settings] =
    Settings.schema.from(Source.of(entries + ("privacy" -> Map("salt" -> salt))))

  test("the bundled config.yml reads back to the schema defaults"):
    val fromEmpty = settingsFrom(Map.empty)
    val fromBundled = Settings.schema.from(BukkitSource(bundled()))
    assert(fromEmpty.isRight && fromBundled.isRight, s"$fromEmpty / $fromBundled")
    assertEquals(fromBundled.map(_.backend), fromEmpty.map(_.backend))
    assertEquals(fromBundled.map(_.sim), fromEmpty.map(_.sim))
    assertEquals(fromBundled.map(_.sink), fromEmpty.map(_.sink))
    assertEquals(fromBundled.map(_.autoLegit), fromEmpty.map(_.autoLegit))
    assertEquals(fromBundled.map(_.shardCompat), fromEmpty.map(_.shardCompat))
    assertEquals(fromBundled.map(_.garnet), fromEmpty.map(_.garnet))
    assertEquals(fromBundled.map(_.exemptBedrock), fromEmpty.map(_.exemptBedrock))
    assertEquals(fromBundled.map(_.alertThreshold), fromEmpty.map(_.alertThreshold))
    assertEquals(fromBundled.map(_.sim.attackPre + fromBundled.toOption.get.sim.attackPost), Right(128))
    assertEquals(fromBundled.map(_.sim.engagePre + fromBundled.toOption.get.sim.engagePost), Right(128))

  test("a remote plaintext endpoint is refused, naming the key"):
    val result = settingsFrom(Map("backend" -> Map("url" -> "ws://eigen.example.com/v1/stream")))
    assertEquals(result.left.toOption, Some("backend.url: plaintext ws:// is allowed only for loopback hosts; use wss://"))

  test("loopback plaintext is allowed for local development"):
    assert(settingsFrom(Map("backend" -> Map("url" -> "ws://127.0.0.1:8787/v1/stream"))).isRight)

  test("an unknown sink names the allowed values"):
    val result = settingsFrom(Map("collect" -> Map("sink" -> "cloud")))
    assertEquals(result.left.toOption, Some("collect.sink: must be one of server, local, both, got 'cloud'"))

  test("a calm segment cannot be configured to end before it starts"):
    val result = settingsFrom(Map("detection" -> Map("calm" -> Map("min-ticks" -> 300, "max-ticks" -> 100))))
    assertEquals(result.left.toOption, Some("detection.calm.max-ticks: must be at least min-ticks"))

  test("the living-target switch maps to the simulation config"):
    val living = settingsFrom(Map("detection" -> Map("targets" -> "living")))
    assertEquals(living.map(_.sim.targetLiving), Right(true))

  test("NPC targets are on by default and switch off on their own"):
    assertEquals(settingsFrom(Map.empty).map(_.sim.targetNpcs), Right(true))
    val off = settingsFrom(Map("detection" -> Map("target-npcs" -> false)))
    assertEquals(off.map(s => (s.sim.targetNpcs, s.sim.targetLiving)), Right((false, false)))

  test("a salt that is too short is rejected with a hint"):
    val result = Settings.schema.from(Source.of(Map("privacy" -> Map("salt" -> "abc"))))
    assertEquals(result.left.toOption, Some("privacy.salt: must be at least 64 hex digits; delete it to generate a new one"))

  test("garnet uri must use redis:// or rediss://"):
    val bad = settingsFrom(Map("garnet" -> Map("uri" -> "http://127.0.0.1:6379")))
    assertEquals(bad.left.toOption, Some("garnet.uri: must be redis:// or rediss://, got 'http://127.0.0.1:6379'"))
    val ok = settingsFrom(Map("garnet" -> Map("enabled" -> true, "uri" -> "rediss://garnet.internal:6379", "server" -> "duels-1")))
    assert(ok.isRight)
    assertEquals(ok.map(_.garnet.server), Right("duels-1"))

  test("collect.auto-legit enables automatic legit session recording"):
    val enabled = settingsFrom(Map("collect" -> Map("auto-legit" -> true)))
    assertEquals(enabled.map(_.autoLegit), Right(true))

  test("collect.shard-compat is disabled by default and configurable"):
    val defaultCfg = settingsFrom(Map.empty)
    assertEquals(defaultCfg.map(_.shardCompat.enabled), Right(false))
    val enabled = settingsFrom(Map("collect" -> Map("shard-compat" -> true, "shard-pre-window" -> 64, "shard-post-window" -> 96)))
    assertEquals(enabled.map(_.shardCompat.enabled), Right(true))
    assertEquals(enabled.map(_.shardCompat.preWindow), Right(64))
    assertEquals(enabled.map(_.shardCompat.postWindow), Right(96))

  test("ServerProtocol selects ModernPing on 1.17+ and LegacyWindowConfirmation on < 1.17"):
    val modern = ServerProtocol.forVersion(ServerVersion.V_1_21_4)
    assertEquals(modern, ServerProtocol.ModernPing)
    assertEquals(modern.livingFlagsIndex, 8)
    assertEquals(modern.outboundTransactionType, PacketType.Play.Server.PING)
    assertEquals(modern.inboundTransactionType, PacketType.Play.Client.PONG)

    val v116 = ServerProtocol.forVersion(ServerVersion.V_1_16_5)
    assertEquals(v116.livingFlagsIndex, 7)
    assertEquals(v116.outboundTransactionType, PacketType.Play.Server.WINDOW_CONFIRMATION)
    assertEquals(v116.inboundTransactionType, PacketType.Play.Client.WINDOW_CONFIRMATION)

    val v112 = ServerProtocol.forVersion(ServerVersion.V_1_12_2)
    assertEquals(v112.livingFlagsIndex, 6)
    assertEquals(v112.outboundTransactionType, PacketType.Play.Server.WINDOW_CONFIRMATION)

    val v18 = ServerProtocol.forVersion(ServerVersion.V_1_8_8)
    assertEquals(v18.livingFlagsIndex, 5)
    assertEquals(v18.outboundTransactionType, PacketType.Play.Server.WINDOW_CONFIRMATION)

  test("PacketTable dispatches in O(1) by enum ordinal and returns null for unrelated packet types"):
    import PacketType.Play.Client as C
    val table = PacketTable.build[C, String](C.values)(
      C.PONG -> "pong",
      C.ATTACK -> "attack",
      C.CLIENT_TICK_END -> "tick_end",
    )
    assertEquals(table.get(C.PONG), "pong")
    assertEquals(table.get(C.ATTACK), "attack")
    assertEquals(table.get(C.CLIENT_TICK_END), "tick_end")
    assertEquals(table.get(C.ANIMATION), null)
    assertEquals(table.get(PacketType.Play.Server.PING), null)
    assertEquals(table.get(PacketType.Configuration.Client.PLUGIN_MESSAGE), null)
