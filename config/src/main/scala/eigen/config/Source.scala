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

package eigen.config

/** One level of a configuration document: scalars by key, and nested sections. */
trait Source:
  /** The scalar stored under `key`, or `None` when absent. */
  def value(key: String): Option[Any]

  /** The nested section under `key`, or `None` when absent or not a section. */
  def section(key: String): Option[Source]

object Source:
  /** A document with no keys: every read falls back to its default. */
  val Empty: Source = of(Map.empty)

  /** A source over nested Scala maps; used for in-memory documents and tests. */
  def of(entries: Map[String, Any]): Source = new MapSource(entries)

  private final class MapSource(entries: Map[String, Any]) extends Source:
    def value(key: String): Option[Any] = entries.get(key).filterNot(_.isInstanceOf[Map[?, ?]])

    def section(key: String): Option[Source] = entries.get(key) match
      case Some(nested: Map[?, ?]) => Some(new MapSource(nested.asInstanceOf[Map[String, Any]]))
      case _ => None
