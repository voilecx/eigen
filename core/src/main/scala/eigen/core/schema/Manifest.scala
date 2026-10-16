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

package eigen.core.schema

import java.nio.charset.StandardCharsets.UTF_8
import java.util.zip.CRC32

/** Canonical manifest JSON of the schema (protocol §4) and its CRC-32. */
object Manifest:

  val json: String =
    val sb = StringBuilder()
    sb.append("{\"version\":").append(Schema.Version).append(",\"layouts\":[")
    for (layout, li) <- Schema.layouts.zipWithIndex do
      if li > 0 then sb.append(',')
      sb.append("{\"id\":").append(layout.id)
      sb.append(",\"name\":\"").append(layout.name).append("\",\"columns\":[")
      for (col, ci) <- layout.cols.zipWithIndex do
        if ci > 0 then sb.append(',')
        sb.append("[\"").append(col.name).append("\",\"").append(col.tpe.wire)
        sb.append("\",\"").append(col.stream.wire).append("\"]")
      sb.append("]}")
    sb.append("]}")
    sb.result()

  val bytes: Array[Byte] = json.getBytes(UTF_8)

  val hash: String =
    val crc = CRC32()
    crc.update(bytes)
    f"${crc.getValue}%08x"
