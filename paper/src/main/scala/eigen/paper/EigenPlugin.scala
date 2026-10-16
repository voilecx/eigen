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

import org.bukkit.plugin.java.JavaPlugin

import eigen.core.schema.Manifest
import eigen.paper.config.Settings

final class EigenPlugin extends JavaPlugin:
  private var eigen: Eigen | Null = null

  override def onEnable(): Unit =
    Settings.load(this) match
      case Left(problem) =>
        getLogger.severe(s"Invalid configuration: $problem. Eigen is disabled.")
        getServer.getPluginManager.disablePlugin(this)
      case Right(settings) =>
        val e = Eigen.wire(this, settings)
        e.start()
        eigen = e
        getLogger.info(s"Eigen ${getPluginMeta.getVersion} enabled (schema ${Manifest.hash})")

  override def onDisable(): Unit =
    val e = eigen
    if e != null then e.stop()
    eigen = null
