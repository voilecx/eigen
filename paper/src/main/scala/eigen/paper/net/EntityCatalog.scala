/*
 * This file is part of Eigen.
 * Copyright (C) 2026 Eigen contributors
 *
 * Portions of this file are derived from the following projects, also licensed under
 * the GNU General Public License version 3:
 *   Shard  - https://github.com/KaelusAI/Shard
 *            Copyright (C) 2026 KaelusAI
 *   GrimAC - https://github.com/GrimAnticheat/Grim
 *            Copyright (c) 2021-2026 GrimAC, DefineOutside and contributors
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

import com.github.retrooper.packetevents.protocol.entity.`type`.{EntityType, EntityTypes}

import eigen.core.entity.{ClientProfile, EntityKind, TrackedEntity}

/**
 * Hitbox sizes and interpolation steps per entity type, as the client uses them. Sizes follow
 * vanilla's entity dimensions; derived from Shard's BoundingBoxSize (GPL-3.0).
 */
object EntityCatalog:

  private final case class Dims(width: Float, height: Float)

  private val dims: Map[String, Dims] =
    def d(w: Float, h: Float, names: String*) = names.map(_ -> Dims(w, h))
    Seq(
      d(0.6f, 1.8f, "player"),
      d(0.6f, 1.95f, "zombie", "zombie_villager", "husk", "drowned", "skeleton", "stray", "bogged",
        "piglin", "piglin_brute", "zombified_piglin", "villager", "wandering_trader", "vindicator",
        "pillager", "evoker", "illusioner", "witch"),
      d(0.6f, 1.8f, "blaze"),
      d(0.6f, 2.9f, "enderman"),
      d(0.6f, 1.77f, "breeze"),
      d(0.7f, 2.4f, "wither_skeleton"),
      d(1.4f, 0.9f, "spider"),
      d(0.7f, 0.5f, "cave_spider"),
      d(0.6f, 1.7f, "creeper"),
      d(1.4f, 2.7f, "iron_golem"),
      d(0.7f, 1.9f, "snow_golem"),
      d(1.95f, 2.2f, "ravager"),
      d(4.0f, 4.0f, "ghast"),
      d(4.0f, 4.5f, "happy_ghast"),
      d(16.0f, 8.0f, "ender_dragon"),
      d(0.9f, 3.5f, "wither"),
      d(0.9f, 2.9f, "warden"),
      d(0.85f, 0.85f, "guardian"),
      d(1.9975f, 1.9975f, "elder_guardian"),
      d(0.6f, 0.85f, "wolf"),
      d(0.9f, 1.4f, "cow", "mooshroom"),
      d(0.9f, 1.3f, "sheep", "goat"),
      d(0.9f, 0.9f, "pig"),
      d(0.9f, 1.875f, "llama", "trader_llama"),
      d(0.9f, 0.6f, "dolphin"),
      d(0.9f, 1.7f, "strider"),
      d(1.3964844f, 1.6f, "horse", "donkey", "mule", "skeleton_horse", "zombie_horse"),
      d(1.3964844f, 1.4f, "hoglin", "zoglin"),
      d(1.3f, 1.25f, "panda"),
      d(0.4f, 0.7f, "chicken"),
      d(0.4f, 0.3f, "endermite", "silverfish"),
      d(0.4f, 0.8f, "vex"),
      d(0.4f, 0.5f, "rabbit"),
      d(0.7f, 0.6f, "bee"),
      d(0.5f, 0.9f, "bat", "parrot"),
      d(0.5f, 0.3f, "cod"),
      d(0.5f, 0.4f, "tropical_fish"),
      d(0.75f, 0.42f, "axolotl"),
      d(0.5f, 0.55f, "frog"),
      d(0.5f, 1.975f, "armor_stand"),
      d(2.0f, 2.0f, "end_crystal"),
      d(0.9f, 0.5f, "phantom"),
    ).flatten.toMap

  private val Fallback = Dims(0.6f, 1.95f)
  private val Boat = Dims(1.375f, 0.5625f)

  private def name(t: EntityType): String = t.getName.getKey

  def kind(t: EntityType): EntityKind =
    if t == EntityTypes.PLAYER then EntityKind.Player
    else if t == EntityTypes.END_CRYSTAL then EntityKind.Crystal
    else if EntityTypes.isTypeInstanceOf(t, EntityTypes.LIVINGENTITY) then EntityKind.Living
    else EntityKind.Other

  /** Interpolation steps the client uses for this type. */
  def steps(t: EntityType, modernBoats: Boolean): Int =
    if EntityTypes.isTypeInstanceOf(t, EntityTypes.BOAT) then
      if modernBoats then TrackedEntity.LivingSteps else TrackedEntity.LegacyBoatSteps
    else if EntityTypes.isTypeInstanceOf(t, EntityTypes.MINECART_ABSTRACT) then TrackedEntity.MinecartSteps
    else if t == EntityTypes.SHULKER then 1
    else if EntityTypes.isTypeInstanceOf(t, EntityTypes.LIVINGENTITY) then TrackedEntity.LivingSteps
    else 1

  /**
   * Applies the hitbox size for the entity's current state. `crouching` and `scale` come from
   * metadata and attributes; `profile` decides how this client sizes a crouching player.
   */
  def resize(e: TrackedEntity, t: EntityType, profile: ClientProfile, crouching: Boolean, lowPose: Boolean, scale: Float): Unit =
    val base =
      if EntityTypes.isTypeInstanceOf(t, EntityTypes.BOAT) then Boat
      else dims.getOrElse(name(t), Fallback)
    val height =
      if t != EntityTypes.PLAYER then base.height
      else if lowPose then 0.6f
      else if crouching then profile.crouchHeight
      else base.height
    e.width = base.width * scale
    e.height = height * scale
