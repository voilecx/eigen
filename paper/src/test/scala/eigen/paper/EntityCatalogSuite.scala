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

package eigen.paper.net

import java.util.Optional

import com.github.retrooper.packetevents.protocol.entity.`type`.EntityType
import com.github.retrooper.packetevents.protocol.player.ClientVersion
import com.github.retrooper.packetevents.resources.ResourceLocation

import eigen.core.entity.{ClientProfile, EntityKind, TrackedEntity}

class EntityCatalogSuite extends munit.FunSuite:

  /** An entity type with an explicit parent chain, as a PacketEvents build defines it. */
  private final class Stub(key: String, parent: EntityType | Null) extends EntityType:
    def getName: ResourceLocation = ResourceLocation("minecraft", key)
    def getId(version: ClientVersion): Int = -1
    def getLegacyId(version: ClientVersion): Int = -1
    def getParent: Optional[EntityType] = Optional.ofNullable(parent)
    def isInstanceOf(t: EntityType): Boolean =
      var p: EntityType | Null = this
      var hit = false
      while p != null && !hit do
        hit = p eq t
        p = p.getParent.orElse(null)
      hit

  // Parent chains as PacketEvents defines them. Building them here keeps the tests free of
  // `EntityTypes`, whose initialisation needs a running PacketEvents.
  private val living = Stub("living_entity", null)
  private val avatar = Stub("avatar", living)
  /** PacketEvents 2.14 (26.x): player and mannequin share the avatar base. */
  private val player = Stub("player", avatar)
  private val mannequin = Stub("mannequin", avatar)
  /** Older PacketEvents (servers before 26.x): no avatar type, the player hangs off living. */
  private val legacyPlayer = Stub("player", living)

  test("players are players and mannequins are NPCs"):
    assertEquals(EntityCatalog.kind(player), EntityKind.Player)
    assertEquals(EntityCatalog.kind(mannequin), EntityKind.Npc)
    assertEquals(EntityCatalog.kind(Stub("some_future_npc", Stub("avatar_like", avatar))), EntityKind.Npc)

  test("without an avatar type (PacketEvents before 26.x) the player is still a player"):
    assertEquals(EntityCatalog.kind(legacyPlayer), EntityKind.Player)
    assert(EntityCatalog.avatar(legacyPlayer))
    assert(!EntityCatalog.descendsFrom(legacyPlayer, "avatar"))
    assert(!EntityCatalog.avatar(Stub("armor_stand", living)))

  test("an NPC is sized and crouches like a player"):
    val e = TrackedEntity(7, EntityKind.Npc, TrackedEntity.LivingSteps, ClientProfile.Modern)
    EntityCatalog.resize(e, mannequin, ClientProfile.Modern, crouching = false, lowPose = false, scale = 1f)
    assertEquals((e.width, e.height), (0.6f, 1.8f))
    EntityCatalog.resize(e, mannequin, ClientProfile.Modern, crouching = true, lowPose = false, scale = 1f)
    assertEquals(e.height, ClientProfile.Modern.crouchHeight)
    EntityCatalog.resize(e, legacyPlayer, ClientProfile.Modern, crouching = false, lowPose = true, scale = 1f)
    assertEquals(e.height, 0.6f)
