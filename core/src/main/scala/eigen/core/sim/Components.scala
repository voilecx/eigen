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

import eigen.core.aim.AxisEstimator
import eigen.core.entity.TrackedEntity

/*
 * Components: plain mutable state with primitive fields, one concern each. Packet events write
 * them; systems (see Systems.scala) read and derive from them once per tick. All of it is
 * confined to the player's network thread.
 */

/** Camera rotation and the per-axis mouse-step estimators. */
final class Rotation:
  var yaw = 0f
  var pitch = 0f
  var lastYaw = 0f
  var lastPitch = 0f
  /** Rotation accumulated over the current tick. */
  var tickYaw = 0.0
  var tickPitch = 0.0
  var rotated = false
  /** The camera was set by the server this tick; its delta is not mouse movement. */
  var forced = false
  /** The delta crossed a jump no mouse can produce (lost packets, desync). */
  var discontinuity = false

  val yawAxis = AxisEstimator()
  val pitchAxis = AxisEstimator()

  val look = Look3()
  val lastYawLook = Look3()
  val lastLook = Look3()

  // Derived each tick by the rotation system.
  var dotsYaw = 0f
  var dotsPitch = 0f
  var residYaw = 0f
  var residPitch = 0f

/** Position and movement state of the player's body. */
final class Body:
  var x = 0.0
  var y = 0.0
  var z = 0.0
  var lastX = 0.0
  var lastY = 0.0
  var lastZ = 0.0
  var onGround = false
  var wasOnGround = false
  var fallDistance = 0.0
  var hasPosition = false

  var sprinting = false
  var sneaking = false
  var swimming = false
  var gliding = false
  var riptide = false
  var inVehicle = false
  var inWater = false

  var scale = 1f
  var interactionRange = 3.0

/** Movement keys. Reported by 1.21.2+ clients, inferred from motion for older ones. */
final class Keys:
  var forward = false
  var backward = false
  var left = false
  var right = false
  var jump = false
  var shift = false
  var sprint = false

  def any: Boolean = forward || backward || left || right || jump

  inline def strafe: Int = (if right then 1 else 0) - (if left then 1 else 0)
  inline def ahead: Int = (if forward then 1 else 0) - (if backward then 1 else 0)

/** Clicks of the current tick and the melee timing state. */
final class Clicks:
  var swing = false
  var use = false
  var attack = false
  var attackTarget = -1
  var prevAttackTarget = -1
  var lastAttackTick = Int.MinValue / 2
  var lastPlaceTick = Int.MinValue / 2
  var lastDigTick = Int.MinValue / 2

  /** Ticks since the attack strength was last reset (vanilla `attackStrengthTicker`). */
  var strengthTicks = 0
  var attackSpeed = 4.0

  // Captured at the moment of the attack, before the reset.
  var strengthAtAttack = 1f
  var sprintAtAttack = false

  def strength(ticks: Int): Float =
    val period = 20.0 / attackSpeed
    math.max(0.0, math.min(1.0, (ticks + 0.5) / period)).toFloat

/** Conditions that invalidate parts of a tick, with their expiry ticks. */
final class Conditions:
  var teleportUntil = Int.MinValue / 2
  var knockbackUntil = Int.MinValue / 2
  var guiOpen = false
  var idleRun = 0

/** Melee target selection and the combat gate. */
final class Combat:
  var target: TrackedEntity | Null = null
  var gate = false
  var lastCombatTick = Int.MinValue / 2
  /** Increments at every engagement; see protocol §5.3. */
  var engagement = 0
  var engagedThisTick = false

  // Target features of the current tick, derived by the target system.
  var aimYaw = 0f
  var aimPitch = 0f
  var aimAngle = 0f
  var distance = 0f
  var relFwd = 0f
  var relSide = 0f
  var relUp = 0f
  var hit = false
  var hitX = 0f
  var hitY = 0f
  var hitZ = 0f
  var miss = 0f
  var crit = false
  var inflation = 0f

/** Item kinds in the hotbar and offhand, for the `item_kind` meta column. */
final class Inventory:
  import ItemKind.ItemKind

  private val hotbar = new Array[Byte](9)
  var offhand: ItemKind = ItemKind.Empty
  var slot = 0

  def setHotbar(index: Int, kind: ItemKind): Unit = hotbar(index) = kind.code
  def held: ItemKind = ItemKind.fromCode(hotbar(slot))