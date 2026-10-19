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

package eigen.shard

import java.util.{ArrayDeque, Queue, UUID}
import java.util.{HashMap as JHashMap, HashSet as JHashSet}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.logging.{Level, Logger}

import com.github.retrooper.packetevents.event.PacketSendEvent
import com.github.retrooper.packetevents.protocol.ConnectionState
import com.github.retrooper.packetevents.protocol.item.`type`.{ItemType, ItemTypes}
import com.github.retrooper.packetevents.protocol.player.{ClientVersion, GameMode, User}
import com.github.retrooper.packetevents.protocol.teleport.RelativeFlag
import com.github.retrooper.packetevents.util.Vector3d
import org.bukkit.entity.Player

final class ShardPacketStateData:
  var packetPlayerOnGround: Boolean = false
  var lastPacketWasTeleport: Boolean = false
  var lastPacketWasServerRotation: Boolean = false
  var lastPacketWasOnePointSeventeenDuplicate: Boolean = false
  var duplicatePacketFilterPosition: Vector3d = Vector3d(0.0, 0.0, 0.0)

  def shouldIgnoreFlyingTick: Boolean =
    lastPacketWasOnePointSeventeenDuplicate ||
      lastPacketWasTeleport ||
      lastPacketWasServerRotation

final class ShardMovementState:
  var x: Double = 0.0
  var y: Double = 0.0
  var z: Double = 0.0
  var yaw: Float = 0f
  var pitch: Float = 0f
  var lastYaw: Float = 0f
  var lastPitch: Float = 0f

final class ShardCombatState(initialTicksSinceAttack: Int = 255):
  var ticksSinceAttack: Int = initialTicksSinceAttack
  @volatile var hasAttacked: Boolean = false

final class ShardCrystalTracker:
  import ShardCrystalTracker.*

  private val crystalSpawn = JHashMap[java.lang.Integer, java.lang.Long]()
  private val anchorUse = JHashMap[java.lang.Long, java.lang.Long]()

  def onCrystalSpawn(entityId: Int, tick: Long): Unit =
    if crystalSpawn.size >= MAX_TRACKED then crystalSpawn.clear()
    crystalSpawn.put(entityId, tick)

  def onEntityRemoved(entityId: Int): Unit =
    crystalSpawn.remove(entityId)

  def spawnToAttack(entityId: Int, tick: Long): Int =
    val spawnTick = crystalSpawn.get(entityId)
    if spawnTick == null then NONE
    else
      val delta = tick - spawnTick.longValue()
      math.max(MIN_DELTA.toLong, math.min(MAX_DELTA.toLong, delta)).toInt

  def anchorUseInterval(x: Int, y: Int, z: Int, tick: Long): Int =
    val key: java.lang.Long =
      ((x.toLong & COORD_MASK) << X_SHIFT) |
        ((z.toLong & COORD_MASK) << Z_SHIFT) |
        (y.toLong & Y_MASK)
    val prev = anchorUse.get(key)
    if anchorUse.size >= MAX_TRACKED then anchorUse.clear()
    anchorUse.put(key, tick)
    if prev == null then NONE
    else math.max(0L, math.min(MAX_DELTA.toLong, tick - prev.longValue())).toInt

  def clear(): Unit =
    crystalSpawn.clear()
    anchorUse.clear()

object ShardCrystalTracker:
  inline val MAX_TRACKED = 256
  inline val NONE = -1
  inline val MIN_DELTA = -1024
  inline val MAX_DELTA = 1024
  inline val COORD_MASK = 0x3ffffffL
  inline val Y_MASK = 0xfffL
  inline val X_SHIFT = 38
  inline val Z_SHIFT = 12

final class ShardTrackingState:
  import ShardTrackingState.*

  var heldSlot: Int = 0
  val hotbarItems: Array[ItemType | Null] = new Array[ItemType | Null](HOTBAR_SIZE)
  var offhandItem: ItemType | Null = null
  var usingOffhand: Boolean = false
  var activeItem: Short = ACTIVE_ITEM_NONE

  @volatile var pendingSequenceBreak: Boolean = false
  @volatile var pendingBufferReset: Boolean = false

  @volatile var sequenceId: Int = 1
  var tickIndex: Long = 0

  var lastY: Double = 0.0
  var firstTickProcessed: Boolean = false
  var rotationThisTick: Boolean = false
  var lastCaptureAtMillis: Long = 0L

  var floodCreditsMs: Double = FLOOD_BURST_MS
  var floodLastNanos: Long = 0L
  var floodDroppedCaptures: Long = 0L

  var sprinting: Boolean = false
  var sneaking: Boolean = false
  var isUsingItem: Boolean = false
  var flying: Boolean = false
  var swimming: Boolean = false
  var gliding: Boolean = false
  var inVehicle: Boolean = false
  var isClimbing: Boolean = false
  var inWater: Boolean = false

  var pose: Int = -1

  var inputForward: Boolean = false
  var inputBackward: Boolean = false
  var inputLeft: Boolean = false
  var inputRight: Boolean = false
  var inputJump: Boolean = false
  var inputShift: Boolean = false
  var inputSprint: Boolean = false
  var inputValid: Boolean = false

  var gameMode: Int = 0
  var foodLevel: Int = 20
  var health: Float = DEFAULT_HEALTH
  var damageTakenThisTick: Float = 0.0f
  var ticksSinceDamage: Int = 255
  var groundFriction: Float = 0.6f
  var fallDistance: Float = 0.0f
  var movementSpeed: Float = 0.1f

  var jumpAmplifier: Int = -1
  var slowFalling: Boolean = false
  var hasBlindness: Boolean = false
  var playerPing: Int = 0

  var speedAmplifier: Int = -1
  var slownessAmplifier: Int = -1
  var hasteAmplifier: Int = -1
  var miningFatigueAmplifier: Int = -1

  var entityInteractionRange: Float = 3.0f
  var scale: Float = 1.0f
  var gravity: Float = 0.08f
  var jumpStrength: Float = 0.42f
  var stepHeight: Float = 0.6f

  var targetEntityId: Int = -1
  var lastTargetEntityId: Int = -1

  var ticksSinceAttack: Int = 255
  var cooldownTicks: Int = 255
  var preAttackCooldownProgress: Float = 1.0f
  var swungThisTick: Boolean = false
  var attackThisTick: Boolean = false
  var windowStartThisTick: Boolean = false
  var windowStartKind: Short = 0
  var enabledWindowStarts: () => Int = () => MELEE_PLAYER_ONLY
  var isSprintingOnAttack: Boolean = false
  var attackSpeed: Float = 4.0f

  var receivedVelocityThisTick: Boolean = false
  var velocityX: Float = 0.0f
  var velocityY: Float = 0.0f
  var velocityZ: Float = 0.0f
  var ticksSinceVelocity: Int = 255
  var receivedExplosionThisTick: Boolean = false
  var ticksSinceExplosion: Int = 255
  var explosionKbX: Float = 0.0f
  var explosionKbY: Float = 0.0f
  var explosionKbZ: Float = 0.0f

  var ticksSinceUseItem: Int = 255

  var slotSwitchesThisTick: Int = 0
  var placesThisTick: Int = 0
  var placeFace: Int = -1
  var placeBlockClass: Int = 0
  var placeBlockX: Int = 0
  var placeBlockY: Int = 0
  var placeBlockZ: Int = 0
  var placeCursorX: Float = -1.0f
  var placeCursorY: Float = -1.0f
  var placeCursorZ: Float = -1.0f
  var placeInsideBlock: Boolean = false

  var crystalSpawnToAttack: Int = -1
  var placeOffhand: Boolean = false
  var attackTargetType: Short = ShardTickData.TARGET_PLAYER
  var anchorCharge: Int = -1
  var anchorUseInterval: Int = -1

  var stuckMultX: Float = 1.0f
  var stuckMultY: Float = 1.0f
  var stuckMultZ: Float = 1.0f

  var onSlime: Boolean = false
  var onHoney: Boolean = false
  var onSoulSand: Boolean = false
  var onMud: Boolean = false

  var levitationAmplifier: Int = -1
  var dolphinsGrace: Boolean = false
  var waterMovementEfficiency: Float = 0.0f
  var riptideActive: Boolean = false

  def onTickStart(): Unit =
    if pendingSequenceBreak then
      pendingSequenceBreak = false
      onSequenceBreak()
    tickIndex += 1

  def onTickEnd(): Unit =
    if ticksSinceAttack < 255 then ticksSinceAttack += 1
    if cooldownTicks < 255 then cooldownTicks += 1
    if ticksSinceVelocity < 255 then ticksSinceVelocity += 1
    if ticksSinceExplosion < 255 then ticksSinceExplosion += 1
    if ticksSinceUseItem < 255 then ticksSinceUseItem += 1
    if ticksSinceDamage < 255 then ticksSinceDamage += 1

    swungThisTick = false
    attackThisTick = false
    windowStartThisTick = false
    windowStartKind = 0
    damageTakenThisTick = 0.0f
    receivedVelocityThisTick = false
    receivedExplosionThisTick = false
    isSprintingOnAttack = false
    rotationThisTick = false
    slotSwitchesThisTick = 0
    placesThisTick = 0
    placeFace = -1
    placeBlockClass = 0
    placeBlockX = 0
    placeBlockY = 0
    placeBlockZ = 0
    placeCursorX = -1.0f
    placeCursorY = -1.0f
    placeCursorZ = -1.0f
    placeInsideBlock = false
    crystalSpawnToAttack = -1
    placeOffhand = false
    attackTargetType = ShardTickData.TARGET_PLAYER
    anchorCharge = -1
    anchorUseInterval = -1

  def onTickAborted(): Unit =
    swungThisTick = false
    attackThisTick = false
    windowStartThisTick = false
    windowStartKind = 0
    damageTakenThisTick = 0.0f
    receivedVelocityThisTick = false
    receivedExplosionThisTick = false
    isSprintingOnAttack = false
    rotationThisTick = false
    slotSwitchesThisTick = 0
    placesThisTick = 0
    placeFace = -1
    placeBlockClass = 0
    placeBlockX = 0
    placeBlockY = 0
    placeBlockZ = 0
    placeCursorX = -1.0f
    placeCursorY = -1.0f
    placeCursorZ = -1.0f
    placeInsideBlock = false
    crystalSpawnToAttack = -1
    placeOffhand = false
    attackTargetType = ShardTickData.TARGET_PLAYER
    anchorCharge = -1
    anchorUseInterval = -1

  def raiseWindowStart(kind: Short): Unit =
    if (enabledWindowStarts() & (1 << kind.toInt)) == 0 then return
    if !windowStartThisTick || kind < windowStartKind then
      windowStartThisTick = true
      windowStartKind = kind

  def onAttack(entityId: Int, isSprinting: Boolean): Unit =
    attackThisTick = true
    lastTargetEntityId = targetEntityId
    targetEntityId = entityId
    isSprintingOnAttack = isSprinting
    val cooldownPeriod = 20.0f / attackSpeed
    preAttackCooldownProgress = math.max(0f, math.min(1f, (cooldownTicks + 0.5f) / cooldownPeriod))
    ticksSinceAttack = 0
    cooldownTicks = 0

  def onSwing(): Unit =
    swungThisTick = true
    cooldownTicks = 0

  def onKnockback(vx: Double, vy: Double, vz: Double): Unit =
    receivedVelocityThisTick = true
    velocityX = vx.toFloat
    velocityY = vy.toFloat
    velocityZ = vz.toFloat
    ticksSinceVelocity = 0

  def onExplosion(kbX: Float, kbY: Float, kbZ: Float): Unit =
    receivedExplosionThisTick = true
    ticksSinceExplosion = 0
    explosionKbX = kbX
    explosionKbY = kbY
    explosionKbZ = kbZ

  def onSlotSwitch(): Unit =
    slotSwitchesThisTick += 1

  def swapOffhand(): Unit =
    val held = if heldSlot >= 0 && heldSlot < hotbarItems.length then hotbarItems(heldSlot) else null
    if heldSlot >= 0 && heldSlot < hotbarItems.length then hotbarItems(heldSlot) = offhandItem
    offhandItem = held
    isUsingItem = false
    updateActiveItem()

  def onBlockPlace(
      face: Int,
      blockClass: Int,
      blockX: Int,
      blockY: Int,
      blockZ: Int,
      cursor: Array[Float],
      insideBlock: Boolean,
      offhand: Boolean,
  ): Unit =
    placesThisTick += 1
    if offhand then placeOffhand = true
    placeFace = face
    placeBlockClass = blockClass
    placeBlockX = blockX
    placeBlockY = blockY
    placeBlockZ = blockZ
    placeCursorX = cursor(0)
    placeCursorY = cursor(1)
    placeCursorZ = cursor(2)
    placeInsideBlock = insideBlock

  def onSequenceBreak(): Unit =
    sequenceId += 1
    usingOffhand = false
    activeItem = ACTIVE_ITEM_NONE
    targetEntityId = -1
    lastTargetEntityId = -1
    firstTickProcessed = false
    lastCaptureAtMillis = 0L
    fallDistance = 0f
    sprinting = false
    sneaking = false
    gliding = false
    swimming = false
    inVehicle = false
    isClimbing = false
    inWater = false
    attackSpeed = 4.0f
    entityInteractionRange = 3.0f
    scale = 1.0f
    gravity = 0.08f
    jumpStrength = 0.42f
    stepHeight = 0.6f
    movementSpeed = 0.1f
    ticksSinceAttack = 255
    cooldownTicks = 255
    health = DEFAULT_HEALTH
    ticksSinceDamage = 255
    damageTakenThisTick = 0.0f
    swungThisTick = false
    attackThisTick = false
    windowStartThisTick = false
    windowStartKind = 0
    damageTakenThisTick = 0.0f
    receivedVelocityThisTick = false
    receivedExplosionThisTick = false
    isSprintingOnAttack = false
    inputForward = false
    inputBackward = false
    inputLeft = false
    inputRight = false
    inputJump = false
    inputShift = false
    inputSprint = false
    inputValid = false

  def floodTryConsume(nowNanos: Long): Boolean =
    if floodLastNanos != 0L then
      val elapsedMs = (nowNanos - floodLastNanos) / 1_000_000.0
      floodCreditsMs = math.min(FLOOD_BURST_MS, floodCreditsMs + elapsedMs)
    floodLastNanos = nowNanos
    if floodCreditsMs < FLOOD_TICK_COST_MS then false
    else
      floodCreditsMs -= FLOOD_TICK_COST_MS
      true

  def buildMovementBitfield(clientOnGround: Boolean): Short =
    var bits = 0
    if clientOnGround then bits |= (1 << 0)
    if sprinting then bits |= (1 << 1)
    if sneaking then bits |= (1 << 2)
    if flying then bits |= (1 << 4)
    if swimming then bits |= (1 << 5)
    if gliding then bits |= (1 << 6)
    if inVehicle then bits |= (1 << 7)
    if isClimbing then bits |= (1 << 8)
    if inWater then bits |= (1 << 9)
    bits.toShort

  def updateActiveItem(): Unit =
    if !isUsingItem then
      activeItem = ACTIVE_ITEM_NONE
      return
    val item =
      if usingOffhand then offhandItem
      else if heldSlot >= 0 && heldSlot < hotbarItems.length then hotbarItems(heldSlot)
      else null
    activeItem =
      if item == null then ACTIVE_ITEM_OTHER
      else if item == ItemTypes.SHIELD then ACTIVE_ITEM_SHIELD
      else if item == ItemTypes.BOW || item == ItemTypes.CROSSBOW then ACTIVE_ITEM_RANGED
      else if item == ItemTypes.TRIDENT then ACTIVE_ITEM_TRIDENT
      else if item.hasAttribute(ItemTypes.ItemAttribute.EDIBLE) then ACTIVE_ITEM_CONSUMABLE
      else if item == ItemTypes.POTION || item == ItemTypes.MILK_BUCKET || item == ItemTypes.HONEY_BOTTLE then ACTIVE_ITEM_CONSUMABLE
      else ACTIVE_ITEM_OTHER

object ShardTrackingState:
  inline val MELEE_PLAYER_ONLY = 1
  inline val DEFAULT_HEALTH = 20.0f
  inline val HOTBAR_SIZE = 9
  final val ACTIVE_ITEM_NONE: Short = 0
  final val ACTIVE_ITEM_SHIELD: Short = 1
  final val ACTIVE_ITEM_CONSUMABLE: Short = 2
  final val ACTIVE_ITEM_RANGED: Short = 3
  final val ACTIVE_ITEM_TRIDENT: Short = 4
  final val ACTIVE_ITEM_OTHER: Short = 5
  private inline val FLOOD_TICK_COST_MS = 50.0
  private inline val FLOOD_BURST_MS = 1000.0

/**
 * The host's single transaction source, shared with Eigen.
 *
 * Implemented in the Paper adapter by `EigenPlayer`: every method delegates to that player's
 * positive `eigen.core.latency.TransactionClock` and to `EigenPlayer.sendTransaction` /
 * `bracket`, so sending a ping synchronously advances `lastSent` through Eigen's outbound
 * listener exactly as Eigen's own sends do. ShardCompat never allocates, sends, consumes or
 * cancels a transaction id of its own; ids `<= 0` stay entirely foreign (GrimAC/other plugins).
 */
trait ShardTransactionSource:
  /** Shared sent/received ordinals (counts, not wire ids). */
  def lastSent: Int
  def lastReceived: Int
  /** Host-smoothed transaction round-trip time in milliseconds (0 until the first answer). */
  def rttMillis: Double
  /** Sends one host transaction ping; `lastSent` has advanced by the time this returns. */
  def sendTransaction(): Unit
  /** Schedules a single trailing transaction after `event` is sent (idempotent across listeners). */
  def sendAfter(event: PacketSendEvent): Unit =
    if event eq null then sendTransaction()
    else event.getTasksAfterSend.add(() => sendTransaction())
  /** Sends one host transaction before `event` (idempotent across listeners on the same `event`). */
  def preTransaction(event: PacketSendEvent): Int =
    val _ = event
    sendTransaction()
    lastSent
  /** Brackets the packet being sent with host transactions; returns the pre-packet ordinal. */
  def bracket(event: PacketSendEvent): Int
  /** Schedules `task` on the shared clock; it runs on the connection network thread at `transaction`. */
  def schedule(transaction: Int, task: Runnable): Unit

/**
 * Shard-side view of the shared clock: ordinal reads, keepalive and despawn bookkeeping.
 * Holds no wire ids and no send/ack state of its own; the last-seen ordinals are only a
 * cache for the moments before the host player exists.
 */
final class ShardTransactionBridge(sourceProvider: () => ShardTransactionSource | Null):
  private val despawned: JHashSet[java.lang.Integer] = JHashSet()
  private var despawnedAtSent: Int = 0
  private val lastSentSeen: AtomicInteger = AtomicInteger(0)
  private val lastReceivedSeen: AtomicInteger = AtomicInteger(0)
  private val lastSentSeenAtMs: AtomicLong = AtomicLong(System.currentTimeMillis())

  def source: ShardTransactionSource | Null = sourceProvider()

  private def noteSent(value: Int): Int =
    if value != lastSentSeen.get() then
      lastSentSeen.set(value)
      lastSentSeenAtMs.set(System.currentTimeMillis())
    value

  def lastSent(): Int =
    val s = source
    if s == null then lastSentSeen.get() else noteSent(s.lastSent)

  def lastReceived(): Int =
    val s = source
    if s == null then lastReceivedSeen.get()
    else
      val value = s.lastReceived
      lastReceivedSeen.set(value)
      value

  def rttMillis: Double =
    val s = source
    if s == null then 0.0 else s.rttMillis

  /** Shared RTT as the integer milliseconds written to Shard's `player_ping` column. */
  def playerPingMillis: Int = math.round(rttMillis).toInt

  def sendTransaction(): Unit =
    val s = source
    if s != null then
      s.sendTransaction()
      val _ = lastSent()

  def sendAfter(event: PacketSendEvent | Null = null): Unit =
    val s = source
    if s == null || event == null then sendTransaction()
    else s.sendAfter(event)

  def preTransaction(event: PacketSendEvent | Null = null): Int =
    val s = source
    if s == null || event == null then
      sendTransaction()
      lastSent()
    else
      val id = s.preTransaction(event)
      val _ = noteSent(s.lastSent)
      id

  def bracketTransaction(event: PacketSendEvent): Int =
    val s = source
    if s == null then preTransaction(null)
    else
      val id = s.bracket(event)
      val _ = noteSent(s.lastSent)
      id

  /** Delegates to the shared clock; false when the host is not bound yet (caller queues). */
  def schedule(transaction: Int, task: Runnable): Boolean =
    val s = source
    if s == null then false
    else
      s.schedule(transaction, task)
      true

  def noteDespawned(entityId: Int): Unit = despawned.synchronized {
    despawned.add(entityId)
    despawnedAtSent = lastSent()
  }

  /** Ordinal for a spawn: forces a fresh shared transaction if this entity was despawned at it. */
  def spawnTransaction(entityId: Int): Int =
    val (isDespawned, atSent) = despawned.synchronized((despawned.contains(entityId), despawnedAtSent))
    if isDespawned then
      if lastSent() <= atSent then sendTransaction()
      despawned.synchronized(despawned.clear())
    lastSent()

  /** Sends a shared keepalive only when no host transaction has been sent recently. */
  def pollKeepalive(user: User | Null): Unit =
    if user == null || user.getEncoderState != ConnectionState.PLAY then return
    if source == null then return
    val _ = lastSent()
    if System.currentTimeMillis() - lastSentSeenAtMs.get() > ShardPlayer.TRANSACTION_KEEPALIVE_MS then sendTransaction()

final class ShardLatencyUtils(private val transactions: ShardTransactionBridge, private val playerName: String, private val logger: Logger):
  private final case class TransactionTask(transactionId: Int, task: Runnable)
  private val pending: ArrayDeque[TransactionTask] = ArrayDeque()

  /**
   * Schedules on the shared clock. The task therefore runs on the connection network thread
   * (from the host clock's pong/drain path) once the client answers `transaction`, never on
   * the sending thread. Tasks queued before the host player exists are transferred, in
   * order, as soon as it is bound.
   */
  def addRealTimeTask(transaction: Int, runnable: Runnable): Unit =
    val task: Runnable = () =>
      try runnable.run()
      catch
        case ex: Exception =>
          logger.log(Level.SEVERE, s"An error occurred when running Shard transactions for player: $playerName", ex)
    flushPending()
    if !transactions.schedule(transaction, task) then pending.synchronized(pending.add(TransactionTask(transaction, task)))

  /** Moves tasks queued before the host was bound onto the shared clock, preserving order. */
  def flushPending(): Unit = pending.synchronized {
    var head = pending.peek()
    while head != null && transactions.schedule(head.transactionId, head.task) do
      pending.poll()
      head = pending.peek()
  }

final class ShardPlayer(
    val user: User | Null,
    val uuid: UUID,
    val name: String,
    val clientVersion: ClientVersion,
    val preWindow: Int,
    val postWindow: Int,
    val logger: Logger,
    val cancelDuplicate: Boolean = true,
    val forceCancelDuplicate: Boolean = false,
    val ignoreDuplicateRotation: Boolean = true,
):
  import ShardPlayer.*

  def this(user: User, preWindow: Int, postWindow: Int, logger: Logger) =
    this(
      user,
      user.getUUID,
      if user.getProfile != null && user.getProfile.getName != null then user.getProfile.getName else user.getUUID.toString,
      if user.getClientVersion != null then user.getClientVersion else ClientVersion.V_1_21_4,
      preWindow,
      postWindow,
      logger,
    )

  @volatile var attachedPlayer: Player | Null = null
  def isAttached: Boolean = attachedPlayer != null

  /** Lazily resolves the host Eigen player's shared clock; bound by the Paper adapter. */
  @volatile var transactionSourceProvider: () => ShardTransactionSource | Null = () => null

  def attach(bukkitPlayer: Player): Unit =
    attachedPlayer = bukkitPlayer

  val packetStateData: ShardPacketStateData = ShardPacketStateData()
  val rotationUpdate: ShardRotationUpdate = ShardRotationUpdate(ShardHeadRotation(), ShardHeadRotation(), 0f, 0f)

  var entityId: Int = 0
  var gameMode: GameMode = GameMode.SURVIVAL
  @volatile var brand: String = "vanilla"
  @volatile var brandReceived: Boolean = false
  var isBedrock: Boolean = false

  val movement: ShardMovementState = ShardMovementState()
  val combat: ShardCombatState = ShardCombatState()
  val tracking: ShardTrackingState = ShardTrackingState()
  val transactions: ShardTransactionBridge = ShardTransactionBridge(() => transactionSourceProvider())

  @volatile var tickBuffer: ShardTickBuffer = ShardTickBuffer(requiredBufferCapacity())

  val pendingTeleports: Queue[TeleportData] = ConcurrentLinkedQueue()
  val pendingRotations: Queue[RotationData] = ConcurrentLinkedQueue()

  val crystalTracker: ShardCrystalTracker = ShardCrystalTracker()
  val compensatedEntities: ShardCompensatedEntities = ShardCompensatedEntities(this)
  val compensatedFireworks: ShardCompensatedFireworks = ShardCompensatedFireworks()
  val compensatedWorld: ShardCompensatedWorld = ShardCompensatedWorld(this)
  val latencyUtils: ShardLatencyUtils = ShardLatencyUtils(transactions, name, logger)
  val aim: ShardAimProcessor = ShardAimProcessor()

  def isPointThree(): Boolean = clientVersion.isOlderThan(ClientVersion.V_1_18_2)
  def getMovementThreshold(): Double = if isPointThree() then 0.03 else 0.0002

  def sendTransaction(): Unit = transactions.sendTransaction()

  def sendAfter(event: PacketSendEvent | Null = null): Unit = transactions.sendAfter(event)

  def lastSentTransaction(): Int = transactions.lastSent()

  def lastReceivedTransaction(): Int = transactions.lastReceived()

  def preTransaction(event: PacketSendEvent | Null = null): Int = transactions.preTransaction(event)

  def bracketTransaction(event: PacketSendEvent): Int = transactions.bracketTransaction(event)

  def spawnTransaction(entityId: Int): Int = transactions.spawnTransaction(entityId)

  /**
   * Maintains `player_ping` from the shared clock. Smoothing differs from legacy Shard:
   * the value is now Eigen's double EMA (weight 0.125 per answered transaction, shared with
   * Eigen's `ping_bucket`) instead of Shard's private integer `(4*prev + sample) / 5`
   * over its own negative-id pings. Same wire column, same millisecond unit.
   */
  def syncPlayerPing(): Unit =
    if transactions.source != null then tracking.playerPing = transactions.playerPingMillis

  def pollData(): Unit = transactions.pollKeepalive(user)

  def ensureTickBufferCapacity(): Unit =
    val required = requiredBufferCapacity()
    if required > tickBuffer.capacity() then tickBuffer = tickBuffer.resizedTo(required)

  private def requiredBufferCapacity(): Int = math.max(1, preWindow + postWindow + 1)

object ShardPlayer:
  inline val TRANSACTION_KEEPALIVE_MS = 80L

  final class TeleportData(
      val location: Vector3d,
      val yaw: Float,
      val pitch: Float,
      val flags: RelativeFlag,
      val transactionId: Int,
  ):
    private inline val FULL_TURN = 360f
    private inline val MIN_PITCH = -90f
    private inline val MAX_PITCH = 90f

    def isRelativeX(): Boolean = flags.has(RelativeFlag.X)
    def isRelativeY(): Boolean = flags.has(RelativeFlag.Y)
    def isRelativeZ(): Boolean = flags.has(RelativeFlag.Z)
    def isRelativePos(): Boolean = isRelativeX() || isRelativeY() || isRelativeZ()

    def rotationMatches(actualYaw: Float, actualPitch: Float): Boolean =
      (flags.has(RelativeFlag.YAW) || angleMatches(actualYaw, yaw, yaw % FULL_TURN)) &&
        (flags.has(RelativeFlag.PITCH) || angleMatches(actualPitch, pitch, math.max(MIN_PITCH, math.min(MAX_PITCH, pitch))))

    private def angleMatches(actual: Float, raw: Float, adjusted: Float): Boolean =
      !java.lang.Float.isFinite(raw) || actual == raw || actual == adjusted

  final class RotationData(
      val yaw: Float,
      val pitch: Float,
      val relativeYaw: Boolean,
      val relativePitch: Boolean,
      val transactionId: Int,
  )
