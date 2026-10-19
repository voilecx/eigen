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

/**
 * Coarse class of a held item, for the `item_kind` meta column. Codes are dataset format.
 *
 * Encapsulated as a zero-cost `opaque type` backed by a primitive `Byte` so item kind codes
 * stay distinct from arbitrary bytes at compile time with zero runtime overhead.
 */
object ItemKind:
  opaque type ItemKind = Byte

  val Empty: ItemKind = 0
  val Sword: ItemKind = 1
  val Axe: ItemKind = 2
  val Mace: ItemKind = 3
  val Trident: ItemKind = 4
  val Spear: ItemKind = 5
  val Bow: ItemKind = 6
  val Crossbow: ItemKind = 7
  val Crystal: ItemKind = 8
  val Totem: ItemKind = 9
  val Shield: ItemKind = 10
  val Food: ItemKind = 11
  val Other: ItemKind = 12

  inline def fromCode(code: Byte): ItemKind = code

  extension (self: ItemKind)
    inline def code: Byte = self
    inline def toFloat: Float = self.toFloat

  private object Suffix:
    def unapply(s: String): Option[String] =
      val idx = s.lastIndexOf('_')
      if idx >= 0 then Some(s.substring(idx)) else None

  /** Classifies a namespaced item id such as `minecraft:diamond_sword`. */
  def of(id: String): ItemKind =
    id.substring(id.indexOf(':') + 1) match
      case "" | "air" => Empty
      case "mace" => Mace
      case "trident" => Trident
      case "bow" => Bow
      case "crossbow" => Crossbow
      case "end_crystal" => Crystal
      case "totem_of_undying" => Totem
      case "shield" => Shield
      case "golden_apple" | "enchanted_golden_apple" | "cooked_beef" | "bread" | "golden_carrot" => Food
      case Suffix("_sword") => Sword
      case Suffix("_axe") => Axe
      case Suffix("_spear") => Spear
      case _ => Other
