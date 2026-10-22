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

package eigen.core.sim

import java.util.ArrayDeque

import eigen.core.entity.{ClientProfile, EntityKind, TrackedEntity}
import eigen.core.history.{CalmTrack, TickHistory, Window}
import eigen.core.geom.Box
import eigen.core.util.IntMap

/**
 * The server's model of one player's client, advanced tick by tick.
 *
 * Packet handlers call the `on*` methods, which only record facts into components. At the end
 * of every client tick [[endTick]] runs the [[Schedule]] of systems, which derive features,
 * write one row into the [[history]] and cut windows that are passed to `emit`. Everything is
 * confined to the player's network thread; other threads talk to it through transaction tasks
 * or the owner's command queue.
 */
final class PlayerSim(val profile: ClientProfile, val config: SimConfig, emit: Window => Unit):
  import PlayerSim.*

  val rotation = Rotation()
  val body = Body()
  val keys = Keys()
  val clicks = Clicks()
  val conditions = Conditions()
  val combat = Combat()
  val inventory = Inventory()

  val entities = IntMap[TrackedEntity](64)
  var selfId: Int = -1

  val history = TickHistory(config.historyCapacity)
  val calm = CalmTrack(config.calmMinTicks, config.calmMaxTicks, config.calmMinActive)
  private[sim] val attackCut = PendingCut()
  private[sim] val engageCut = PendingCut()

  /** Client tick index of the tick being built; see protocol §5.2. */
  var tick: Int = 0
  var epoch: Int = 1
  var pingBucket: Int = 0

  /** When false, rows are still recorded but no windows are cut (nobody consumes them). */
  var windowsWanted: Boolean = false

  private val pendingTeleports = ArrayDeque[PendingTeleport]()
  private val pendingRotations = ArrayDeque[PendingRotation]()

  // Scratch space shared by systems; never escapes a call.
  private[sim] val boxA = Box()
  private[sim] val boxB = Box()
  private[sim] val boxC = Box()
  private[sim] val pair = new Array[Double](3)

  /** Whether this tick has seen a (non-duplicate) movement packet; read by `RecordSystem`. */
  private[sim] var flyingThisTick = false

  // ---- client → server events -------------------------------------------------------------

  /**
   * A movement packet. For clients without tick-end packets it also closes the tick, unless it
   * is the extra mid-tick packet 1.17–1.20 clients send when using an item.
   */
  def onFlying(hasPos: Boolean, x: Double, y: Double, z: Double, hasRot: Boolean, yaw: Float, pitch: Float, onGround: Boolean): Unit =
    val duplicate = profile.duplicateFlying && hasPos && hasRot && body.hasPosition &&
      onGround == body.onGround &&
      math.abs(x - body.x) < profile.movementThreshold &&
      math.abs(y - body.y) < profile.movementThreshold &&
      math.abs(z - body.z) < profile.movementThreshold

    if hasPos && hasRot && acceptTeleport(x, y, z, yaw, pitch) then
      setPosition(x, y, z, resetMotion = true)
      setRotation(yaw, pitch)
      rotation.forced = true
      conditions.teleportUntil = tick + config.teleportTicks
    else if hasRot && !hasPos && acceptServerRotation(yaw, pitch) then
      setRotation(yaw, pitch)
      rotation.forced = true
    else
      if hasPos then setPosition(x, y, z, resetMotion = false)
      if hasRot then rotate(yaw, pitch)

    body.onGround = onGround
    if !duplicate then flyingThisTick = true
    if !profile.tickEnd && !duplicate then endTick()

  /** `CLIENT_TICK_END` (1.21.2+). */
  def onTickEnd(): Unit = if profile.tickEnd then endTick()

  def onInput(forward: Boolean, backward: Boolean, left: Boolean, right: Boolean, jump: Boolean, shift: Boolean, sprint: Boolean): Unit =
    if profile.realInput then
      keys.forward = forward; keys.backward = backward
      keys.left = left; keys.right = right
      keys.jump = jump; keys.shift = shift; keys.sprint = sprint
      body.sneaking = shift

  def onSwing(): Unit = clicks.swing = true
  def onUse(): Unit = clicks.use = true

  def onPlace(): Unit =
    clicks.use = true
    clicks.lastPlaceTick = tick

  def onDig(): Unit = clicks.lastDigTick = tick

  def onAttack(entityId: Int): Unit =
    if entityId == selfId then return
    clicks.attack = true
    clicks.prevAttackTarget = if clicks.lastAttackTick > Int.MinValue / 2 then clicks.attackTarget else -1
    clicks.attackTarget = entityId
    clicks.lastAttackTick = tick
    clicks.strengthAtAttack = clicks.strength(clicks.strengthTicks)
    clicks.sprintAtAttack = body.sprinting
    clicks.strengthTicks = 0

  def onSlot(slot: Int): Unit =
    if slot >= 0 && slot < 9 && slot != inventory.slot then
      inventory.slot = slot
      clicks.strengthTicks = 0

  def onSprint(sprinting: Boolean): Unit = body.sprinting = sprinting
  def onSneak(sneaking: Boolean): Unit = if !profile.realInput then body.sneaking = sneaking
  def onGui(open: Boolean): Unit = conditions.guiOpen = open

  // ---- server → client facts, applied once the client acknowledged them -------------------

  def expectTeleport(t: PendingTeleport): Unit =
    if pendingTeleports.size >= MaxPending then pendingTeleports.pollFirst()
    pendingTeleports.addLast(t)

  def expectRotation(r: PendingRotation): Unit =
    if pendingRotations.size >= MaxPending then pendingRotations.pollFirst()
    pendingRotations.addLast(r)

  def onKnockback(): Unit = conditions.knockbackUntil = tick + config.knockbackTicks

  def onSelfFlags(gliding: Boolean, swimming: Boolean): Unit =
    body.gliding = gliding
    body.swimming = swimming

  def onSelfLivingFlags(riptide: Boolean): Unit = body.riptide = riptide
  def onVehicle(inVehicle: Boolean): Unit = body.inVehicle = inVehicle
  def onAttackSpeed(value: Double): Unit = clicks.attackSpeed = math.max(0.1, value)
  def onInteractionRange(value: Double): Unit = body.interactionRange = value
  def onSelfScale(value: Float): Unit = body.scale = value

  def addEntity(e: TrackedEntity): Unit = entities(e.id) = e
  def entity(id: Int): TrackedEntity | Null = entities.getOrNull(id)
  inline def withEntity(id: Int)(inline f: TrackedEntity => Unit): Unit = entities.withValue(id)(f)
  def removeEntity(id: Int): Unit =
    val e = entities.remove(id)
    if e != null && combat.target == e then combat.target = null

  /**
   * The player's timeline broke (join, respawn, world change): flush what was being built and
   * start a new epoch, so no window spans the break.
   */
  def breakEpoch(): Unit =
    WindowSystem.flushCalm(this)
    attackCut.active = false
    engageCut.active = false
    entities.clear()
    combat.target = null
    pendingTeleports.clear()
    pendingRotations.clear()
    body.hasPosition = false
    epoch += 1

  // ---- tick -------------------------------------------------------------------------------

  def endTick(): Unit =
    Schedule.run(this)
    flyingThisTick = false
    tick += 1

  private[sim] inline def emitWindow(w: Window): Unit = emit(w)

  // ---- internals --------------------------------------------------------------------------

  private def setPosition(x: Double, y: Double, z: Double, resetMotion: Boolean): Unit =
    if resetMotion || !body.hasPosition then
      body.lastX = x; body.lastY = y; body.lastZ = z
    body.x = x; body.y = y; body.z = z
    body.hasPosition = true

  private def setRotation(yaw: Float, pitch: Float): Unit =
    rotation.lastYaw = yaw; rotation.lastPitch = pitch
    rotation.yaw = yaw; rotation.pitch = pitch

  private def rotate(yaw: Float, pitch: Float): Unit =
    val dy = yaw.toDouble - rotation.yaw
    val dp = pitch.toDouble - rotation.pitch
    if math.abs(dy) > 180 || math.abs(dp) > 180 then rotation.discontinuity = true
    else
      rotation.tickYaw += dy
      rotation.tickPitch += dp
    rotation.rotated = rotation.rotated || dy != 0 || dp != 0
    rotation.lastYaw = rotation.yaw; rotation.lastPitch = rotation.pitch
    rotation.yaw = yaw; rotation.pitch = pitch

  private def acceptTeleport(x: Double, y: Double, z: Double, yaw: Float, pitch: Float): Boolean =
    val it = pendingTeleports.iterator()
    var matched = -1
    var i = 0
    while matched < 0 && it.hasNext do
      if it.next().matches(x, y, z, yaw, pitch, body, rotation, profile.movementThreshold) then matched = i
      i += 1
    if matched >= 0 then for _ <- 0 to matched do pendingTeleports.pollFirst()
    matched >= 0

  private def acceptServerRotation(yaw: Float, pitch: Float): Boolean =
    val head = pendingRotations.peekFirst()
    if head != null && head.matches(yaw, pitch) then
      pendingRotations.pollFirst()
      true
    else false

  /** Whether entities of this kind count as melee targets under the current configuration. */
  private[sim] def targetable(kind: EntityKind): Boolean =
    kind == EntityKind.Player ||
      (config.targetNpcs && kind == EntityKind.Npc) ||
      (config.targetLiving && kind == EntityKind.Living)

object PlayerSim:
  private inline val MaxPending = 8

/** A server teleport the client has acknowledged and should answer with a matching movement. */
final class PendingTeleport(
    x: Double, y: Double, z: Double, yaw: Float, pitch: Float,
    relX: Boolean, relY: Boolean, relZ: Boolean, relYaw: Boolean, relPitch: Boolean,
):
  def matches(ax: Double, ay: Double, az: Double, ayaw: Float, apitch: Float, body: Body, rot: Rotation, threshold: Double): Boolean =
    val ex = x + (if relX then body.x else 0.0)
    val ey = y + (if relY then body.y else 0.0)
    val ez = z + (if relZ then body.z else 0.0)
    val tol = (if relX || relY || relZ then threshold else 0.0) + 1e-6
    val eyaw = yaw + (if relYaw then rot.yaw else 0f)
    val epitch = pitch + (if relPitch then rot.pitch else 0f)
    math.abs(ax - ex) <= tol && math.abs(ay - ey) <= tol + 1e-7 && math.abs(az - ez) <= tol &&
      math.abs(ayaw - eyaw) <= 1e-3f && math.abs(apitch - math.max(-90f, math.min(90f, epitch))) <= 1e-3f

/** A rotation the server set (1.21.2+ `PLAYER_ROTATION`); relative axes accept any value. */
final class PendingRotation(yaw: Float, pitch: Float, relYaw: Boolean, relPitch: Boolean):
  def matches(ayaw: Float, apitch: Float): Boolean =
    (relYaw || ayaw == yaw) && (relPitch || apitch == pitch)