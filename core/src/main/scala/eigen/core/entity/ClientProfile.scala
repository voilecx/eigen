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

package eigen.core.entity

/**
 * Behaviour of a player's client version that the simulation must mirror. Built once per
 * player by the platform adapter from the protocol version; the simulation never inspects the
 * version itself.
 *
 * Old clients reach a modern server through ViaVersion, which translates packets but not the
 * client's own physics, timing and rounding — those are what this profile describes.
 */
final case class ClientProfile(
    protocol: Int,
    /** 1.21.2+: the client sends `CLIENT_TICK_END`, so every client tick is observed. */
    tickEnd: Boolean,
    /** 1.21.2+: `PLAYER_INPUT` carries the real movement keys. */
    realInput: Boolean,
    /** 1.9+ without tick-end packets: the client may skip a movement packet on idle ticks. */
    skipsIdleTicks: Boolean,
    /** 1.17–1.20.x: using an item sends an extra position-and-rotation packet mid-tick. */
    duplicateFlying: Boolean,
    /** Below 1.19.3 relative moves are packed with `floor`, not `round`. */
    floorPacking: Boolean,
    /** 1.8: positions are 1/32-block fixed point, so boxes carry that rounding as jitter. */
    coarsePositions: Boolean,
    /** 1.14.4 < v < 1.20.2 and 1.21.4 < v < 1.21.9: rotation-only packets freeze interpolation. */
    rotationFreezesInterpolation: Boolean,
    /** 1.21.11+: the trigonometry table is indexed in double precision. */
    doubleIndexTrig: Boolean,
    /** 1.8 checks two look vectors for a hit, 1.9+ three (a tick can lag behind). */
    lookCandidates: Int,
    /** Eye height while sneaking: 1.54 before 1.14, 1.27 since. */
    sneakEyeHeight: Double,
    /** Hitbox height of a crouching player as this client sees it: 1.8 before 1.14, 1.5 since. */
    crouchHeight: Float,
    /** Smallest position change the client reports: 0.03 before 1.18.2, 0.0002 since. */
    movementThreshold: Double,
)

object ClientProfile:
  /** The profile of a current client: everything observed, nothing legacy. */
  val Modern: ClientProfile = ClientProfile(
    protocol = Int.MaxValue,
    tickEnd = true,
    realInput = true,
    skipsIdleTicks = false,
    duplicateFlying = false,
    floorPacking = false,
    coarsePositions = false,
    rotationFreezesInterpolation = false,
    doubleIndexTrig = true,
    lookCandidates = 3,
    sneakEyeHeight = 1.27,
    crouchHeight = 1.5f,
    movementThreshold = 0.0002,
  )
