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

package eigen.paper.alert

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*

import net.kyori.adventure.text.Component
import net.kyori.adventure.text.format.NamedTextColor
import org.bukkit.Bukkit
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

import eigen.core.wire.Messages
import eigen.paper.config.Settings
import eigen.paper.garnet.{AlertBroadcast, GarnetBridge}
import eigen.paper.net.Players

/** Staff notifications for backend verdicts. Staff opt in with `/eigen alerts`. */
final class Alerts(settings: Settings, players: Players, garnet: GarnetBridge, plugin: JavaPlugin):
  private val listening = ConcurrentHashMap.newKeySet[UUID]()

  /** Toggles alerts for `staff`; returns whether they are now on. */
  def toggle(staff: Player): Boolean =
    if listening.remove(staff.getUniqueId) then false
    else
      listening.add(staff.getUniqueId)
      true

  /** Handles a verdict from the stream thread. */
  def onVerdict(v: Messages.Verdict): Unit =
    val suspect = v.scores.iterator.filter((label, _) => label != "legit").maxByOption(_._2)
    val p = players.byHandleOrNull(v.handle)
    suspect.foreach: (label, score) =>
      if score >= settings.alertThreshold && p != null then
        val playerName = p.name
        garnet.recordViolation(playerName, label, score, v.model): status =>
          val message = formatAlert(None, playerName, label, score, v.model, status.vl)
          Bukkit.getGlobalRegionScheduler.execute(plugin, () => broadcast(message))

  /** Handles an alert published by another Paper instance over Garnet Pub/Sub. */
  def onRemoteAlert(a: AlertBroadcast): Unit =
    val message = formatAlert(Some(a.server), a.player, a.label, a.score, a.model, a.vl)
    Bukkit.getGlobalRegionScheduler.execute(plugin, () => broadcast(message))

  private def formatAlert(
      remoteServer: Option[String],
      player: String,
      label: String,
      score: Double,
      model: String,
      vl: Long,
  ): Component =
    val prefix = remoteServer.fold("[Eigen] ")(srv => s"[Eigen:$srv] ")
    Component
      .text(prefix, NamedTextColor.DARK_AQUA)
      .append(Component.text(player, NamedTextColor.WHITE))
      .append(Component.text(f" $label ${score * 100}%.1f%% ($model) ", NamedTextColor.GRAY))
      .append(Component.text(s"[x$vl]", NamedTextColor.AQUA))

  private def broadcast(message: Component): Unit =
    Bukkit.getOnlinePlayers.asScala
      .filter(s => listening.contains(s.getUniqueId) && s.hasPermission("eigen.alerts"))
      .foreach(_.sendMessage(message))
    plugin.getComponentLogger.info(message)