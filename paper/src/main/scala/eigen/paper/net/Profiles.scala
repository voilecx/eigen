/*
 * This file is part of Eigen.
 * Copyright (C) 2026 Eigen contributors
 *
 * Portions of this file are derived from the following projects, also licensed under
 * the GNU General Public License version 3:
 *   GrimAC - https://github.com/GrimAnticheat/Grim
 *            Copyright (c) 2021-2026 GrimAC, DefineOutside and contributors
 *   Shard  - https://github.com/KaelusAI/Shard
 *            Copyright (C) 2026 KaelusAI
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

package eigen.paper.net

import com.github.retrooper.packetevents.protocol.player.ClientVersion
import com.github.retrooper.packetevents.protocol.player.ClientVersion.*

import eigen.core.entity.ClientProfile

/** Maps a client's protocol version to the behaviour the simulation must mirror. */
object Profiles:

  def of(v: ClientVersion): ClientProfile =
    inline def atLeast(o: ClientVersion) = v.isNewerThanOrEquals(o)
    inline def below(o: ClientVersion) = v.isOlderThan(o)
    val tickEnd = atLeast(V_1_21_2)
    ClientProfile(
      protocol = v.getProtocolVersion,
      tickEnd = tickEnd,
      realInput = tickEnd,
      skipsIdleTicks = !tickEnd && atLeast(V_1_9),
      duplicateFlying = atLeast(V_1_17) && below(V_1_21),
      floorPacking = below(V_1_19_3),
      coarsePositions = below(V_1_9),
      rotationFreezesInterpolation =
        (v.isNewerThan(V_1_14_4) && below(V_1_20_2)) || (v.isNewerThan(V_1_21_4) && below(V_1_21_9)),
      doubleIndexTrig = atLeast(V_1_21_11),
      lookCandidates = if below(V_1_9) then 2 else 3,
      sneakEyeHeight = if below(V_1_14) then 1.54 else 1.27,
      crouchHeight = if below(V_1_14) then 1.8f else 1.5f,
      movementThreshold = if below(V_1_18_2) then 0.03 else 0.0002,
    )
