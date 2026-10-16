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

import org.bukkit.configuration.ConfigurationSection

import eigen.config.Source

/** Presents a Bukkit configuration section as a platform-free [[Source]]. */
final class BukkitSource(section: ConfigurationSection) extends Source:
  def value(key: String): Option[Any] =
    Option(section.get(key)).filterNot(_.isInstanceOf[ConfigurationSection])

  def section(key: String): Option[Source] =
    Option(section.getConfigurationSection(key)).map(BukkitSource(_))
