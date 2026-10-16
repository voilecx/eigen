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

package eigen.core.sim

/** Coarse class of a held item, for the `item_kind` meta column. Codes are dataset format. */
object ItemKind:
  final val Empty: Byte = 0
  final val Sword: Byte = 1
  final val Axe: Byte = 2
  final val Mace: Byte = 3
  final val Trident: Byte = 4
  final val Spear: Byte = 5
  final val Bow: Byte = 6
  final val Crossbow: Byte = 7
  final val Crystal: Byte = 8
  final val Totem: Byte = 9
  final val Shield: Byte = 10
  final val Food: Byte = 11
  final val Other: Byte = 12

  /** Classifies a namespaced item id such as `minecraft:diamond_sword`. */
  def of(id: String): Byte =
    val name = id.substring(id.indexOf(':') + 1)
    if name.isEmpty || name == "air" then Empty
    else if name.endsWith("_sword") then Sword
    else if name.endsWith("_axe") then Axe
    else if name.endsWith("_spear") then Spear
    else
      name match
        case "mace" => Mace
        case "trident" => Trident
        case "bow" => Bow
        case "crossbow" => Crossbow
        case "end_crystal" => Crystal
        case "totem_of_undying" => Totem
        case "shield" => Shield
        case "golden_apple" | "enchanted_golden_apple" | "cooked_beef" | "bread" | "golden_carrot" => Food
        case _ => Other
