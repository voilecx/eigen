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

package eigen.paper.collect

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger

/** Labels a collection session can carry (protocol §3.3). */
enum Label(val wire: String):
  case Legit extends Label("legit")
  case Cheat extends Label("cheat")
  case Unknown extends Label("unknown")

object Label:
  def parse(s: String): Option[Label] = values.find(_.wire == s.toLowerCase)

/** One recording of one player, from `collect start` to `collect stop`. */
final class CollectSession(
    val id: Int,
    val uuid: UUID,
    val handle: Int,
    val playerName: String,
    val player: String,
    val label: Label,
    val tags: Map[String, String],
    val note: String,
    val startedAt: Long,
    val selfService: Boolean = false,
):
  val windows = AtomicInteger()
  @volatile var cheatActive: Boolean = label == Label.Cheat