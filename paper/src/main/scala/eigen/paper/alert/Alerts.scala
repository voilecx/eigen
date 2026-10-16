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
import eigen.paper.net.Players

/** Staff notifications for backend verdicts. Staff opt in with `/eigen alerts`. */
final class Alerts(settings: Settings, players: Players, plugin: JavaPlugin):
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
        val message = Component
          .text("[Eigen] ", NamedTextColor.DARK_AQUA)
          .append(Component.text(p.name, NamedTextColor.WHITE))
          .append(Component.text(f" $label ${score * 100}%.1f%% (${v.model})", NamedTextColor.GRAY))
        Bukkit.getGlobalRegionScheduler.execute(plugin, () => broadcast(message))

  private def broadcast(message: Component): Unit =
    Bukkit.getOnlinePlayers.asScala
      .filter(s => listening.contains(s.getUniqueId) && s.hasPermission("eigen.alerts"))
      .foreach(_.sendMessage(message))
    plugin.getComponentLogger.info(message)
