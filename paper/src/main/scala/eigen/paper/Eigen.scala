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

package eigen.paper

import com.github.retrooper.packetevents.PacketEvents
import io.papermc.paper.threadedregions.scheduler.ScheduledTask
import org.bukkit.Bukkit
import org.bukkit.plugin.java.JavaPlugin

import eigen.commands.CommandTree
import eigen.paper.alert.Alerts
import eigen.paper.cmd.EigenCommands
import eigen.paper.collect.{Collector, LocalSink}
import eigen.paper.config.Settings
import eigen.paper.net.{Inbound, Outbound, Players}
import eigen.paper.stream.StreamClient

/**
 * The plugin's object graph. Built at compile time by MacWire's `autowire` in [[Eigen.wire]]:
 * every service receives its dependencies through its constructor, with no runtime container.
 */
final class Eigen(
    plugin: JavaPlugin,
    stream: StreamClient,
    local: LocalSink,
    players: Players,
    collector: Collector,
    alerts: Alerts,
    inbound: Inbound,
    outbound: Outbound,
    commands: EigenCommands,
):
  private var heartbeat: ScheduledTask | Null = null

  def start(): Unit =
    stream.onVerdict = alerts.onVerdict
    stream.start()
    val events = PacketEvents.getAPI.getEventManager
    events.registerListener(inbound)
    events.registerListener(outbound)
    CommandTree.register(plugin, commands.spec)
    // One transaction per server tick keeps acknowledgements flowing while nothing else is sent.
    heartbeat = Bukkit.getGlobalRegionScheduler.runAtFixedRate(plugin, _ => players.all.foreach(_.sendTransaction()), 1L, 1L)

  def stop(): Unit =
    val h = heartbeat
    if h != null then h.cancel()
    val events = PacketEvents.getAPI.getEventManager
    events.unregisterListener(inbound)
    events.unregisterListener(outbound)
    collector.stopAll()
    stream.stop()
    local.shutdown()

object Eigen:
  import com.softwaremill.macwire.autowire

  def wire(plugin: JavaPlugin, settings: Settings): Eigen =
    val stream = StreamClient(
      settings.backend,
      s"eigen ${plugin.getPluginMeta.getVersion}",
      s"${Bukkit.getName} ${Bukkit.getMinecraftVersion}",
      plugin.getLogger,
    )
    val local = LocalSink(plugin.getDataFolder.toPath.resolve("collect"), plugin.getLogger)
    autowire[Eigen](plugin, settings, stream, local)
