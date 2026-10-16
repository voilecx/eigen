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

package eigen.core.geom

object Angles:

  inline val DegToRad = 0.017453292519943295
  inline val RadToDeg = 57.29577951308232

  /** Wraps an angle in degrees into [-180, 180). */
  inline def wrapDegrees(deg: Double): Double =
    val r = deg % 360.0
    if r >= 180.0 then r - 360.0
    else if r < -180.0 then r + 360.0
    else r

  /** Signed shortest turn from `from` to `to`, in degrees. */
  inline def turn(from: Double, to: Double): Double = wrapDegrees(to - from)
