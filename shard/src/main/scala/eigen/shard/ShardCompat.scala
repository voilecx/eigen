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

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.{ArrayList, Optional, ServiceLoader, UUID}
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, ExecutorService, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import java.util.logging.{Level, Logger}

import scala.jdk.CollectionConverters.*

import com.github.retrooper.packetevents.PacketEvents
import com.github.retrooper.packetevents.event.*
import com.github.retrooper.packetevents.manager.server.ServerVersion
import com.github.retrooper.packetevents.protocol.entity.data.EntityData
import com.github.retrooper.packetevents.protocol.entity.pose.EntityPose
import com.github.retrooper.packetevents.protocol.entity.`type`.{EntityType, EntityTypes}
import com.github.retrooper.packetevents.protocol.item.`type`.{ItemType, ItemTypes}
import com.github.retrooper.packetevents.protocol.packettype.PacketType
import com.github.retrooper.packetevents.protocol.player.{ClientVersion, DiggingAction, GameMode, InteractionHand, User}
import com.github.retrooper.packetevents.protocol.potion.{PotionType, PotionTypes}
import com.github.retrooper.packetevents.protocol.teleport.RelativeFlag
import com.github.retrooper.packetevents.protocol.world.Location
import com.github.retrooper.packetevents.protocol.world.states.`type`.StateTypes
import com.github.retrooper.packetevents.util.Vector3d
import com.github.retrooper.packetevents.wrapper.configuration.client.WrapperConfigClientPluginMessage
import com.github.retrooper.packetevents.wrapper.play.client.*
import com.github.retrooper.packetevents.wrapper.play.server.*
import io.papermc.paper.threadedregions.scheduler.ScheduledTask
import org.bukkit.Bukkit
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

/** Configuration for the optional Shard 2 compatibility dataset collector. */
final case class ShardCompatConfig(
    enabled: Boolean = false,
    preWindow: Int = 128,
    postWindow: Int = 128,
)

/**
 * Pluggable interface for the parallel Shard 2 dataset collector.
 *
 * When `collect.shard-compat.enabled` is `false` (the default), [[ShardCompat.Noop]] is used:
 * no listeners are registered, no `ShardPlayer` / `ShardTickBuffer` instances are allocated,
 * and zero GC pressure is added to the server.
 */
trait ShardCompat:
  def enabled: Boolean
  def start(): Unit
  def stop(): Unit
  /** Wires the host's shared transaction source (Eigen's `TransactionClock` per player). */
  def bindTransactionSource(lookup: User => ShardTransactionSource | Null): Unit = ()
  def startSession(playerUuid: UUID, playerName: String, label: String, details: String, startedAtMs: Long, eigenDir: Path | Null): Unit
  def stopSession(playerUuid: UUID): Unit

trait ShardCompatProvider:
  def create(plugin: JavaPlugin, root: Path, config: ShardCompatConfig, log: Logger): ShardCompat

object ShardCompat:
  object Noop extends ShardCompat:
    def enabled: Boolean = false
    def start(): Unit = ()
    def stop(): Unit = ()
    def startSession(playerUuid: UUID, playerName: String, label: String, details: String, startedAtMs: Long, eigenDir: Path | Null): Unit = ()
    def stopSession(playerUuid: UUID): Unit = ()

  /** Loads the Shard collector module via `ServiceLoader` only when enabled in config. */
  def load(plugin: JavaPlugin, root: Path, config: ShardCompatConfig, log: Logger): ShardCompat =
    if !config.enabled then Noop
    else
      val loader = ServiceLoader.load(classOf[ShardCompatProvider], classOf[ShardCompat].getClassLoader)
      loader.iterator().asScala.nextOption() match
        case Some(provider) => provider.create(plugin, root, config, log)
        case None =>
          new DefaultShardCompatProvider().create(plugin, root, config, log)

final class DefaultShardCompatProvider extends ShardCompatProvider:
  def create(plugin: JavaPlugin, root: Path, config: ShardCompatConfig, log: Logger): ShardCompat =
    new ActiveShardCompat(plugin, root, config, log)

/**
 * Exact 1:1 port of Shard's `CollectSession` (`w0.csv`, `w1.csv`, ..., `session.meta`).
 * Writes into `plugins/Eigen/collect-shard/<LABEL>_<player>_<timestamp>/` (and optionally mirrors
 * into a `shard/` subdirectory inside the active Eigen local session directory when present).
 */
final class ShardCollectSession(
    val sessionId: UUID,
    val playerUuid: UUID,
    val playerName: String,
    val label: String,
    val serverVersion: String,
    val clientVersion: Int,
    val startTime: Instant,
    val eigenDir: Path | Null,
):
  import ShardCollectSession.*

  private val pendingWindows = ConcurrentLinkedQueue[ArrayList[ShardTickData]]()
  private val windowCount = AtomicInteger(0)

  def addWindow(window: ArrayList[ShardTickData]): Unit =
    pendingWindows.add(window)
    windowCount.incrementAndGet()

  def count(): Int = windowCount.get()

  def save(collectRoot: Path): Unit =
    val windows = ArrayList[ArrayList[ShardTickData]]()
    var w = pendingWindows.poll()
    while w != null do
      windows.add(w)
      w = pendingWindows.poll()
    if windows.isEmpty then return

    val finalDir =
      val ed = eigenDir
      if ed != null then ed.resolve("shard")
      else collectRoot.resolve(generateSessionDirName())
    val stagingDir = collectRoot.resolve(STAGING_DIR).resolve(sessionId.toString)
    deleteRecursively(stagingDir)
    Files.createDirectories(stagingDir)

    var totalTicks = 0
    var writtenIdx = 0
    var idx = 0
    while idx < windows.size() do
      val window = windows.get(idx)
      if !window.isEmpty then
        val writer = Files.newBufferedWriter(stagingDir.resolve(s"w$writtenIdx.csv"), UTF_8)
        try
          writer.append(ShardTickSchema.csvHeader).append('\n')
          var tIdx = 0
          while tIdx < window.size() do
            ShardTickSchema.appendCsvRow(writer, window.get(tIdx))
            writer.append('\n')
            tIdx += 1
        finally writer.close()
        totalTicks += window.size()
        writtenIdx += 1
      idx += 1

    val metaWriter = Files.newBufferedWriter(stagingDir.resolve("session.meta"), UTF_8)
    try
      metaWriter.append("session_id=").append(sessionId.toString).append('\n')
      metaWriter.append("player_uuid=").append(playerUuid.toString).append('\n')
      metaWriter.append("player_name=").append(playerName).append('\n')
      metaWriter.append("label=").append(label).append('\n')
      metaWriter.append("format_version=1\n")
      metaWriter.append("schema_version=").append(SCHEMA_VERSION.toString).append('\n')
      metaWriter.append("collection_timestamp=").append(startTime.toEpochMilli.toString).append('\n')
      metaWriter.append("server_version=").append(serverVersion).append('\n')
      metaWriter.append("client_version=").append(clientVersion.toString).append('\n')
      metaWriter.append("windows=").append(windows.size().toString).append('\n')
      metaWriter.append("ticks=").append(totalTicks.toString).append('\n')
    finally metaWriter.close()

    if finalDir.getParent != null then Files.createDirectories(finalDir.getParent)
    deleteRecursively(finalDir)
    try Files.move(stagingDir, finalDir, StandardCopyOption.ATOMIC_MOVE)
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(stagingDir, finalDir, StandardCopyOption.REPLACE_EXISTING)

  private def generateSessionDirName(): String =
    val timestamp = TIMESTAMP_FORMAT.format(startTime)
    val safeLabel = label.replace(' ', '_').replaceAll("[/\\\\?%*:|\"<>']", "-")
    s"${safeLabel}_${playerName}_${timestamp}"

object ShardCollectSession:
  private inline val SCHEMA_VERSION = 1
  private inline val STAGING_DIR = ".staging"
  private val TIMESTAMP_FORMAT = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss").withZone(ZoneId.systemDefault())

  def formatShardLabel(label: String, details: String): String =
    val upper = label.toUpperCase(java.util.Locale.ROOT)
    val trimmed = details.trim
    if trimmed.isEmpty then s"$upper default" else s"$upper $trimmed"

  def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream.sorted(java.util.Comparator.reverseOrder()).forEach(p => { val _ = Files.deleteIfExists(p) })
      finally stream.close()

final class ActiveShardCompat(
    plugin: JavaPlugin,
    collectRoot: Path,
    config: ShardCompatConfig,
    log: Logger,
) extends ShardCompat:

  @volatile private var transactionHost: User => ShardTransactionSource | Null = _ => null

  override def bindTransactionSource(lookup: User => ShardTransactionSource | Null): Unit =
    transactionHost = lookup

  private val players = ConcurrentHashMap[User, ShardPlayer]()
  private val activeSessions = ConcurrentHashMap[UUID, ShardCollectSession]()
  private val trackers = ConcurrentHashMap[UUID, ShardAttackWindowTracker]()
  private val io: ExecutorService = Executors.newSingleThreadExecutor(r =>
    val t = Thread(r, "eigen-shard-io")
    t.setDaemon(true)
    t,
  )

  private val receiveListener = new PacketListenerAbstract(PacketListenerPriority.NORMAL):
    override def onUserLogin(event: UserLoginEvent): Unit =
      val user: User = event.getUser
      val bukkit: Player = event.getPlayer
      if user != null && bukkit != null then handleUserLogin(user, bukkit)

    override def onUserDisconnect(event: UserDisconnectEvent): Unit =
      val user = event.getUser
      if user != null then handleUserDisconnect(user)

    override def onPacketReceive(event: PacketReceiveEvent): Unit =
      val sp = players.get(event.getUser)
      if sp != null then handlePacketReceive(event, sp)

  private val sendListener = new PacketListenerAbstract(PacketListenerPriority.MONITOR):
    override def onPacketSend(event: PacketSendEvent): Unit =
      if !event.isCancelled then handlePacketSend(event)

  private var pollTask: ScheduledTask | Null = null

  def enabled: Boolean = true

  def start(): Unit =
    val events = PacketEvents.getAPI.getEventManager
    events.registerListener(receiveListener)
    events.registerListener(sendListener)
    pollTask = Bukkit.getGlobalRegionScheduler.runAtFixedRate(
      plugin,
      _ => players.values().asScala.foreach(_.pollData()),
      1L,
      1L,
    )
    log.info(s"Shard 2 compatibility collector enabled (pre=${config.preWindow}, post=${config.postWindow})")

  def stop(): Unit =
    val pt = pollTask
    pollTask = null
    if pt != null then pt.cancel()
    val events = PacketEvents.getAPI.getEventManager
    events.unregisterListener(receiveListener)
    events.unregisterListener(sendListener)
    activeSessions.keys().asScala.toSeq.foreach(stopSessionSync)
    players.clear()
    io.shutdown()
    val _ = io.awaitTermination(10, TimeUnit.SECONDS)

  def startSession(
      playerUuid: UUID,
      playerName: String,
      label: String,
      details: String,
      startedAtMs: Long,
      eigenDir: Path | Null,
  ): Unit =
    val sp = findPlayer(playerUuid)
    val clientVer = if sp != null then sp.clientVersion.getProtocolVersion else 0
    if sp != null then
      sp.tracking.pendingSequenceBreak = true
      sp.tracking.pendingBufferReset = true
    val session = ShardCollectSession(
      sessionId = UUID.randomUUID(),
      playerUuid = playerUuid,
      playerName = playerName,
      label = ShardCollectSession.formatShardLabel(label, details),
      serverVersion = Bukkit.getVersion,
      clientVersion = clientVer,
      startTime = Instant.ofEpochMilli(startedAtMs),
      eigenDir = eigenDir,
    )
    activeSessions.put(playerUuid, session)
    trackers.put(playerUuid, ShardAttackWindowTracker())

  def stopSession(playerUuid: UUID): Unit =
    val session = activeSessions.remove(playerUuid)
    trackers.remove(playerUuid)
    if session != null then
      io.execute: () =>
        try session.save(collectRoot)
        catch case e: Exception => log.log(Level.SEVERE, s"Failed to save Shard compat data for ${session.playerName}", e)

  private def stopSessionSync(playerUuid: UUID): Unit =
    val session = activeSessions.remove(playerUuid)
    trackers.remove(playerUuid)
    if session != null then
      try session.save(collectRoot)
      catch case e: Exception => log.log(Level.SEVERE, s"Failed to save Shard compat data for ${session.playerName}", e)

  private def findPlayer(uuid: UUID): ShardPlayer | Null =
    players.values().asScala.find(_.uuid == uuid).orNull

  private def handleUserConnect(user: User): Unit =
    if user.getUUID == null || players.containsKey(user) then return
    val sp = new ShardPlayer(user, config.preWindow, config.postWindow, log)
    // Resolved lazily: the host EigenPlayer for this user is created alongside this player.
    sp.transactionSourceProvider = () => transactionHost(user)
    players.put(user, sp)

  private def handleUserLogin(user: User, player: Player): Unit =
    handleUserConnect(user)
    val sp = players.get(user)
    if sp == null || sp.isAttached then return
    sp.entityId = if user.getEntityId > 0 then user.getEntityId else player.getEntityId
    val gm = GameMode.getById(player.getGameMode.getValue)
    if gm != null then
      sp.gameMode = gm
      sp.tracking.gameMode = gm.ordinal
    sp.compensatedWorld.updateMinHeight(player.getWorld.getMinHeight)
    sp.isBedrock = player.getUniqueId.getMostSignificantBits == 0L
    sp.attach(player)

  private def handleUserDisconnect(user: User): Unit =
    val sp = players.remove(user)
    if sp != null then stopSession(sp.uuid)

  // ---- Packet receive pipeline (exact Shard PacketListener + FlyingProcessor) ----

  private def handlePacketReceive(event: PacketReceiveEvent, sp: ShardPlayer): Unit =
    if !sp.isAttached then
      handleBrand(sp, event)
      return

    // Transaction answers are owned by Eigen's clock (positive ids, consumed and cancelled
    // in Eigen's inbound listener); `<= 0` answers are foreign and never touched here.
    // Shared-clock tasks run from that clock on this same network thread.
    sp.syncPlayerPing()
    sp.latencyUtils.flushPending()

    val isFlying = WrapperPlayClientPlayerFlying.isFlying(event.getPacketType)
    if isFlying then sp.tracking.onTickStart()

    event.getPacketType match
      case c: PacketType.Play.Client => routeReceive(c, event, sp)
      case _ => ()

    if isFlying then handleFlying(event, sp)

    if event.isCancelled then
      if isFlying then sp.tracking.onTickAborted()
      resetFlyingFlags(sp)
      return

    syncServerState(event, sp)

    if isFlying then onDataTick(sp)

    handleBrand(sp, event)

    if isFlying then sp.tracking.onTickEnd()

    resetFlyingFlags(sp)

  private def routeReceive(c: PacketType.Play.Client, event: PacketReceiveEvent, sp: ShardPlayer): Unit =
    import PacketType.Play.Client as C
    c match
      case C.INTERACT_ENTITY =>
        val interact = WrapperPlayClientInteractEntity(event)
        if interact.getAction == WrapperPlayClientInteractEntity.InteractAction.ATTACK then
          onAttack(interact.getEntityId, sp)
      case C.ATTACK =>
        onAttack(WrapperPlayClientAttack(event).getEntityId, sp)
      case C.PLAYER_BLOCK_PLACEMENT =>
        onPlacement(event, sp)
      case C.ANIMATION =>
        if WrapperPlayClientAnimation(event).getHand == InteractionHand.MAIN_HAND then sp.tracking.onSwing()
      case C.USE_ITEM =>
        sp.tracking.ticksSinceUseItem = 0
      case C.PLAYER_DIGGING =>
        val dig = WrapperPlayClientPlayerDigging(event)
        if dig.getAction == DiggingAction.RELEASE_USE_ITEM then
          sp.tracking.isUsingItem = false
          sp.tracking.updateActiveItem()
        if dig.getAction == DiggingAction.SWAP_ITEM_WITH_OFFHAND then
          sp.tracking.swapOffhand()
          refreshAttackSpeed(sp)
      case C.HELD_ITEM_CHANGE =>
        val tracking = sp.tracking
        tracking.isUsingItem = false
        tracking.updateActiveItem()
        tracking.cooldownTicks = 0
        tracking.heldSlot = WrapperPlayClientHeldItemChange(event).getSlot
        tracking.onSlotSwitch()
        refreshAttackSpeed(sp)
      case C.ENTITY_ACTION =>
        val tracking = sp.tracking
        WrapperPlayClientEntityAction(event).getAction match
          case WrapperPlayClientEntityAction.Action.START_SPRINTING => tracking.sprinting = true
          case WrapperPlayClientEntityAction.Action.STOP_SPRINTING => tracking.sprinting = false
          case WrapperPlayClientEntityAction.Action.START_SNEAKING => tracking.sneaking = true
          case WrapperPlayClientEntityAction.Action.STOP_SNEAKING => tracking.sneaking = false
          case WrapperPlayClientEntityAction.Action.START_FLYING_WITH_ELYTRA => tracking.gliding = true
          case _ => ()
      case C.PLAYER_ABILITIES =>
        sp.tracking.flying = WrapperPlayClientPlayerAbilities(event).isFlying
      case C.PLAYER_INPUT =>
        val tracking = sp.tracking
        val input = WrapperPlayClientPlayerInput(event)
        tracking.inputForward = input.isForward
        tracking.inputBackward = input.isBackward
        tracking.inputLeft = input.isLeft
        tracking.inputRight = input.isRight
        tracking.inputJump = input.isJump
        tracking.inputShift = input.isShift
        tracking.inputSprint = input.isSprint
        tracking.inputValid = sp.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_21_2)
        if sp.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_21_6) then
          tracking.sneaking = input.isShift
      case C.STEER_VEHICLE =>
        val tracking = sp.tracking
        val steer = WrapperPlayClientSteerVehicle(event)
        tracking.inputForward = steer.getForward > 0f
        tracking.inputBackward = steer.getForward < 0f
        tracking.inputLeft = steer.getSideways > 0f
        tracking.inputRight = steer.getSideways < 0f
        tracking.inputJump = steer.isJump
        tracking.inputShift = steer.isUnmount
        tracking.inputValid = true
      case C.VEHICLE_MOVE =>
        if sp.tracking.inVehicle then
          val pos = WrapperPlayClientVehicleMove(event).getPosition
          sp.movement.x = pos.getX
          sp.movement.y = pos.getY
          sp.movement.z = pos.getZ
      case _ => ()

  private def onAttack(targetId: Int, player: ShardPlayer): Unit =
    if targetId == player.entityId then return
    val target = player.compensatedEntities.getEntity(targetId)
    val tracking = player.tracking
    tracking.crystalSpawnToAttack = player.crystalTracker.spawnToAttack(targetId, tracking.tickIndex)
    tracking.attackTargetType = classifyTarget(target)
    if target != null && target.isPlayer then
      tracking.onAttack(targetId, tracking.sprinting)
      tracking.raiseWindowStart(ShardTickData.START_MELEE_PLAYER)
      return
    tracking.raiseWindowStart(
      if target == null then ShardTickData.START_ATTACK_ENTITY_OTHER
      else if target.tpe == EntityTypes.END_CRYSTAL then ShardTickData.START_ATTACK_END_CRYSTAL
      else if target.isLivingEntity then ShardTickData.START_MELEE_LIVING_OTHER
      else ShardTickData.START_ATTACK_ENTITY_OTHER
    )

  private def classifyTarget(target: ShardPacketEntity | Null): Short =
    if target == null then ShardTickData.TARGET_UNKNOWN
    else if target.isPlayer then ShardTickData.TARGET_PLAYER
    else if target.tpe == EntityTypes.END_CRYSTAL then ShardTickData.TARGET_END_CRYSTAL
    else if target.isLivingEntity then ShardTickData.TARGET_LIVING_OTHER
    else if EntityTypes.isTypeInstanceOf(target.tpe, EntityTypes.BOAT) ||
      EntityTypes.isTypeInstanceOf(target.tpe, EntityTypes.MINECART)
    then ShardTickData.TARGET_VEHICLE
    else ShardTickData.TARGET_OTHER

  private def onPlacement(event: PacketReceiveEvent, player: ShardPlayer): Unit =
    val placement = WrapperPlayClientPlayerBlockPlacement(event)
    val tracking = player.tracking
    tracking.ticksSinceUseItem = 0
    val pos = placement.getBlockPosition
    val state = player.compensatedWorld.getBlock(pos.getX, pos.getY, pos.getZ)
    val blockClass =
      if state == null then ShardTickData.PLACE_UNKNOWN
      else if state.getType == StateTypes.OBSIDIAN || state.getType == StateTypes.CRYING_OBSIDIAN then ShardTickData.PLACE_OBSIDIAN
      else if state.getType == StateTypes.BEDROCK then ShardTickData.PLACE_BEDROCK
      else if state.getType == StateTypes.RESPAWN_ANCHOR then ShardTickData.PLACE_RESPAWN_ANCHOR
      else ShardTickData.PLACE_OTHER_SOLID
    val cursor = placement.getCursorPosition
    tracking.onBlockPlace(
      placement.getFaceId,
      blockClass.toInt,
      pos.getX,
      pos.getY,
      pos.getZ,
      Array(cursor.getX, cursor.getY, cursor.getZ),
      placement.getInsideBlock.orElse(false),
      placement.getHand == InteractionHand.OFF_HAND,
    )
    if state != null && state.getType == StateTypes.RESPAWN_ANCHOR then
      tracking.anchorCharge = state.getCharges
      tracking.anchorUseInterval = player.crystalTracker.anchorUseInterval(pos.getX, pos.getY, pos.getZ, tracking.tickIndex)
      tracking.raiseWindowStart(ShardTickData.START_USE_RESPAWN_ANCHOR)
    else if blockClass == ShardTickData.PLACE_OBSIDIAN || blockClass == ShardTickData.PLACE_BEDROCK then
      val held =
        if tracking.heldSlot >= 0 && tracking.heldSlot < tracking.hotbarItems.length then tracking.hotbarItems(tracking.heldSlot)
        else null
      if held == ItemTypes.END_CRYSTAL then tracking.raiseWindowStart(ShardTickData.START_PLACE_END_CRYSTAL)

  private def handleFlying(event: PacketReceiveEvent, player: ShardPlayer): Unit =
    val flying = WrapperPlayClientPlayerFlying(event)
    val teleported = acceptTeleport(player, flying)
    val serverRotated = !teleported && acceptRotation(player, flying)

    player.packetStateData.lastPacketWasTeleport = teleported
    player.packetStateData.lastPacketWasServerRotation = serverRotated

    if teleported || serverRotated then
      player.tracking.firstTickProcessed = false

    applyDuplicateFilter(player, flying)

    if !event.isCancelled then
      if !teleported && !serverRotated then processRotation(player, flying)
      updateBlockContext(player)
      if !teleported && !player.packetStateData.lastPacketWasOnePointSeventeenDuplicate then
        val reliable = player.clientVersion.isOlderThan(ClientVersion.V_1_9)
        val iter = player.compensatedEntities.entityMap.values().iterator()
        while iter.hasNext do iter.next().onMovement(tickingReliably = reliable)

    if !teleported then
      player.packetStateData.packetPlayerOnGround = flying.isOnGround

  private def applyDuplicateFilter(player: ShardPlayer, flying: WrapperPlayClientPlayerFlying): Unit =
    if player.packetStateData.lastPacketWasTeleport || player.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_21) then return
    val location: Location = flying.getLocation
    val threshold = player.getMovementThreshold()
    val inVehicle = player.compensatedEntities.self.riding != null
    val hasMovementAndRotation = flying.hasPositionChanged && flying.hasRotationChanged
    val sameGroundAndCloseClaim =
      flying.isOnGround == player.packetStateData.packetPlayerOnGround &&
        player.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_17) &&
        player.packetStateData.duplicatePacketFilterPosition.distanceSquared(location.getPosition) < threshold * threshold
    if hasMovementAndRotation && (sameGroundAndCloseClaim || inVehicle) then
      player.packetStateData.lastPacketWasOnePointSeventeenDuplicate = true
      if !player.ignoreDuplicateRotation then
        val m = player.movement
        if m.yaw != location.getYaw || m.pitch != location.getPitch then
          m.lastYaw = m.yaw
          m.lastPitch = m.pitch
        m.yaw = location.getYaw
        m.pitch = location.getPitch

  private def processRotation(player: ShardPlayer, packet: WrapperPlayClientPlayerFlying): Unit =
    val ignoreRotation = player.packetStateData.lastPacketWasOnePointSeventeenDuplicate && player.ignoreDuplicateRotation
    val movement = player.movement
    val loc = packet.getLocation

    if packet.hasPositionChanged then
      movement.x = loc.getX
      movement.y = loc.getY
      movement.z = loc.getZ
      if !player.packetStateData.lastPacketWasOnePointSeventeenDuplicate then
        player.packetStateData.duplicatePacketFilterPosition = loc.getPosition

    if packet.hasRotationChanged && !ignoreRotation then
      player.tracking.rotationThisTick = true
      val newYaw = loc.getYaw
      val newPitch = loc.getPitch
      val update = player.rotationUpdate
      update.from.yaw = movement.yaw
      update.from.pitch = movement.pitch
      update.to.yaw = newYaw
      update.to.pitch = newPitch
      update.deltaYaw = newYaw - movement.yaw
      update.deltaPitch = newPitch - movement.pitch
      player.aim.process(update)
      movement.lastYaw = movement.yaw
      movement.lastPitch = movement.pitch
      movement.yaw = newYaw
      movement.pitch = newPitch

  private def updateBlockContext(player: ShardPlayer): Unit =
    val tracking = player.tracking
    val m = player.movement
    val world = player.compensatedWorld

    val footX = math.floor(m.x).toInt
    val footZ = math.floor(m.z).toInt
    val frictionY = math.floor(m.y - 0.5000001).toInt
    val feetY = math.floor(m.y).toInt
    val bodyY = math.floor(m.y + 0.5).toInt
    val headY = math.floor(m.y + 1.6).toInt

    val frictionBlock = world.getBlock(footX, frictionY, footZ)
    if frictionBlock != null then
      val tpe = frictionBlock.getType
      tracking.groundFriction = ShardBlockFriction.of(tpe)
      tracking.onSlime = ShardBlockFriction.isSlime(tpe)
      tracking.onHoney = ShardBlockFriction.isHoney(tpe)
      tracking.onSoulSand = ShardBlockFriction.isSoulSand(tpe)
      tracking.onMud = ShardBlockFriction.isMud(tpe)
    else
      tracking.onSlime = false
      tracking.onHoney = false
      tracking.onSoulSand = false
      tracking.onMud = false

    val feetState = world.getBlock(footX, feetY, footZ)
    if feetState != null then
      tracking.isClimbing = ShardBlockFriction.isClimbable(feetState.getType)
      tracking.inWater = ShardBlockFriction.isWater(feetState)

    tracking.pose =
      if tracking.gliding then EntityPose.FALL_FLYING.ordinal
      else if tracking.swimming then EntityPose.SWIMMING.ordinal
      else if tracking.riptideActive then EntityPose.SPIN_ATTACK.ordinal
      else if tracking.sneaking then EntityPose.CROUCHING.ordinal
      else EntityPose.STANDING.ordinal

    var stuckX = 1.0f
    var stuckY = 1.0f
    var stuckZ = 1.0f
    val ys = Array(feetY, bodyY, headY)
    var i = 0
    while i < ys.length do
      val b = world.getBlock(footX, ys(i), footZ)
      if b != null then
        val stuck = ShardBlockFriction.stuckMultiplier(b.getType)
        if stuck != null then
          if math.abs(stuck(0) - 1f) > math.abs(stuckX - 1f) then stuckX = stuck(0)
          if math.abs(stuck(1) - 1f) > math.abs(stuckY - 1f) then stuckY = stuck(1)
          if math.abs(stuck(2) - 1f) > math.abs(stuckZ - 1f) then stuckZ = stuck(2)
      i += 1
    tracking.stuckMultX = stuckX
    tracking.stuckMultY = stuckY
    tracking.stuckMultZ = stuckZ

  private def syncServerState(event: PacketReceiveEvent, player: ShardPlayer): Unit =
    val state = player.packetStateData
    if !state.lastPacketWasTeleport && !state.lastPacketWasServerRotation then return
    val flying = WrapperPlayClientPlayerFlying(event)
    val loc = flying.getLocation
    val movement = player.movement
    if flying.hasPositionChanged then
      movement.x = loc.getX
      movement.y = loc.getY
      movement.z = loc.getZ
      state.duplicatePacketFilterPosition = loc.getPosition
    if flying.hasRotationChanged then
      movement.yaw = loc.getYaw
      movement.pitch = loc.getPitch
      movement.lastYaw = movement.yaw
      movement.lastPitch = movement.pitch

  private def onDataTick(player: ShardPlayer): Unit =
    player.syncPlayerPing()
    val tracking = player.tracking
    if tracking.pendingBufferReset then
      tracking.pendingBufferReset = false
      player.tickBuffer.resetForSession()
    player.ensureTickBufferCapacity()
    val withinBudget = tracking.floodTryConsume(System.nanoTime()) || tracking.windowStartThisTick
    if !withinBudget then tracking.floodDroppedCaptures += 1
    val skipped = (player.packetStateData.shouldIgnoreFlyingTick && !tracking.windowStartThisTick) || !withinBudget
    if skipped then return

    val buf = player.tickBuffer
    buf.capture(player)
    if tracking.windowStartThisTick then buf.markAttack()
    onCollectTick(player)
    buf.advance()
    player.compensatedFireworks.onTickEnd()

  private def onCollectTick(player: ShardPlayer): Unit =
    val uuid = player.uuid
    val session = activeSessions.get(uuid)
    if session == null then
      trackers.remove(uuid)
      return
    val tracker = trackers.computeIfAbsent(uuid, _ => ShardAttackWindowTracker())
    tracker.onTick(
      player.tickBuffer,
      player.tracking.windowStartThisTick && player.tracking.windowStartKind == ShardTickData.START_MELEE_PLAYER,
      player.tracking.windowStartKind,
      config.postWindow,
    ): (ticks, attackIndex, kind) =>
      val window = player.tickBuffer.extractWindow(config.preWindow, config.postWindow, ticks, attackIndex, kind)
      if window != null then session.addWindow(window)

  private def resetFlyingFlags(player: ShardPlayer): Unit =
    player.packetStateData.lastPacketWasOnePointSeventeenDuplicate = false
    player.packetStateData.lastPacketWasTeleport = false
    player.packetStateData.lastPacketWasServerRotation = false

  private def acceptTeleport(player: ShardPlayer, flying: WrapperPlayClientPlayerFlying): Boolean =
    if !flying.hasPositionChanged || !flying.hasRotationChanged then return false
    val queue = player.pendingTeleports
    val received = player.lastReceivedTransaction()
    var head = queue.peek()
    while head != null && received > head.transactionId && !landsOn(player, flying, head) do
      queue.poll()
      head = queue.peek()
    val accepted = head != null && received >= head.transactionId && landsOn(player, flying, head)
    if accepted then queue.poll()
    accepted

  private def landsOn(player: ShardPlayer, flying: WrapperPlayClientPlayerFlying, teleport: ShardPlayer.TeleportData): Boolean =
    val movement = player.movement
    val flags = teleport.flags
    val at = flying.getLocation
    val expectedX = teleport.location.getX + (if flags.has(RelativeFlag.X) then movement.x else 0.0)
    val expectedY = teleport.location.getY + (if flags.has(RelativeFlag.Y) then movement.y else 0.0)
    val expectedZ = teleport.location.getZ + (if flags.has(RelativeFlag.Z) then movement.z else 0.0)
    val threshold = if teleport.isRelativePos() then player.getMovementThreshold() else 0.0
    math.abs(at.getX - expectedX) <= threshold &&
      math.abs(at.getY - expectedY) <= 1.0e-7 + threshold &&
      math.abs(at.getZ - expectedZ) <= threshold &&
      teleport.rotationMatches(at.getYaw, at.getPitch)

  private def acceptRotation(player: ShardPlayer, flying: WrapperPlayClientPlayerFlying): Boolean =
    if !flying.hasRotationChanged || flying.hasPositionChanged then return false
    val queue = player.pendingRotations
    val received = player.lastReceivedTransaction()
    var head = queue.peek()
    while head != null && received > head.transactionId && !matchesServerRotation(head, flying) do
      queue.poll()
      head = queue.peek()
    val accepted = head != null && received >= head.transactionId && matchesServerRotation(head, flying)
    if accepted then queue.poll()
    accepted

  private def matchesServerRotation(rotation: ShardPlayer.RotationData, flying: WrapperPlayClientPlayerFlying): Boolean =
    val loc = flying.getLocation
    (rotation.relativeYaw || loc.getYaw == rotation.yaw) &&
      (rotation.relativePitch || loc.getPitch == rotation.pitch)

  private def handleBrand(player: ShardPlayer, event: PacketReceiveEvent): Unit =
    val (ch, data) = event.getPacketType match
      case PacketType.Play.Client.PLUGIN_MESSAGE =>
        val p = WrapperPlayClientPluginMessage(event)
        (p.getChannelName, p.getData)
      case PacketType.Configuration.Client.PLUGIN_MESSAGE =>
        val p = WrapperConfigClientPluginMessage(event)
        (p.getChannelName, p.getData)
      case _ => return
    if (ch == "minecraft:brand" || ch == "MC|Brand") && !player.brandReceived && data != null && data.nonEmpty && data.length <= 64 then
      player.brand = String(data, 1, data.length - 1, UTF_8).replace(" (Velocity)", "")
      player.brandReceived = true

  // ---- Packet send pipeline (exact Shard PacketRoutes SEND_ROUTES) ----

  private def handlePacketSend(event: PacketSendEvent): Unit =
    if event.getPacketType == PacketType.Login.Server.LOGIN_SUCCESS then
      val user = event.getUser
      if user != null then event.getTasksAfterSend.add(() => handleUserConnect(user))
      return
    val sp = players.get(event.getUser)
    if sp == null then return
    sp.latencyUtils.flushPending()
    event.getPacketType match
      case s: PacketType.Play.Server => routeSend(s, event, sp)
      case _ => ()

  @annotation.nowarn("cat=deprecation")
  private def routeSend(s: PacketType.Play.Server, event: PacketSendEvent, player: ShardPlayer): Unit =
    import PacketType.Play.Server as S
    // PING / WINDOW_CONFIRMATION sends are the host clock's business (Eigen's outbound
    // listener marks shared pings sent); ShardCompat allocates and marks nothing itself.
    s match
      case S.ENTITY_VELOCITY =>
        val vel = WrapperPlayServerEntityVelocity(event)
        if vel.getEntityId == player.entityId then
          val v = vel.getVelocity
          player.latencyUtils.addRealTimeTask(
            player.bracketTransaction(event),
            () => player.tracking.onKnockback(v.getX, v.getY, v.getZ),
          )
      case S.EXPLOSION =>
        val kb = WrapperPlayServerExplosion(event).getPlayerMotion
        if kb != null && (kb.getX != 0f || kb.getY != 0f || kb.getZ != 0f) then
          player.latencyUtils.addRealTimeTask(
            player.bracketTransaction(event),
            () => {
              player.tracking.onExplosion(kb.getX, kb.getY, kb.getZ)
              player.tracking.raiseWindowStart(ShardTickData.START_EXPLOSION_RECEIVED)
            },
          )
      case S.ENTITY_EFFECT =>
        val eff = WrapperPlayServerEntityEffect(event)
        if eff.getEntityId == player.entityId then
          val pt = eff.getPotionType
          val amp = eff.getEffectAmplifier
          player.latencyUtils.addRealTimeTask(player.bracketTransaction(event), () => applyPotion(player.tracking, pt, amp))
      case S.REMOVE_ENTITY_EFFECT =>
        val rem = WrapperPlayServerRemoveEntityEffect(event)
        if rem.getEntityId == player.entityId then
          val pt = rem.getPotionType
          player.latencyUtils.addRealTimeTask(player.bracketTransaction(event), () => clearPotion(player.tracking, pt))
      case S.PLAYER_ABILITIES =>
        val flying = WrapperPlayServerPlayerAbilities(event).isFlying
        player.latencyUtils.addRealTimeTask(player.bracketTransaction(event), () => player.tracking.flying = flying)
      case S.UPDATE_HEALTH =>
        val u = WrapperPlayServerUpdateHealth(event)
        val food = u.getFood
        val newHealth = u.getHealth
        player.latencyUtils.addRealTimeTask(
          player.bracketTransaction(event),
          () => {
            val t = player.tracking
            val lost = t.health - newHealth
            if lost > 0f then
              t.damageTakenThisTick = lost
              t.ticksSinceDamage = 0
            t.health = newHealth
            t.foodLevel = food
          },
        )
      case S.CHANGE_GAME_STATE =>
        val gs = WrapperPlayServerChangeGameState(event)
        if gs.getReason == WrapperPlayServerChangeGameState.Reason.CHANGE_GAME_MODE then
          player.tracking.gameMode = gs.getValue.toInt
      case S.JOIN_GAME =>
        val join = WrapperPlayServerJoinGame(event)
        player.tracking.onSequenceBreak()
        val gm = join.getGameMode
        player.entityId = join.getEntityId
        player.gameMode = gm
        player.tracking.gameMode = gm.ordinal
        player.compensatedEntities.clear()
        player.compensatedWorld.clear()
        if PacketEvents.getAPI.getServerManager.getVersion.isNewerThanOrEquals(ServerVersion.V_1_17) then
          try player.compensatedWorld.updateMinHeight(join.getDimensionType.getMinY(player.clientVersion))
          catch case _: Throwable => ()
        event.getTasksAfterSend.add(() => player.sendTransaction())
      case S.RESPAWN =>
        val respawn = WrapperPlayServerRespawn(event)
        player.tracking.onSequenceBreak()
        player.gameMode = respawn.getGameMode
        player.tracking.gameMode = respawn.getGameMode.ordinal
        val minY =
          try Some(respawn.getDimensionType.getMinY(player.clientVersion))
          catch case _: Throwable => None
        player.latencyUtils.addRealTimeTask(
          player.bracketTransaction(event),
          () => {
            player.compensatedEntities.clear()
            player.compensatedWorld.clear()
            minY.foreach(player.compensatedWorld.updateMinHeight)
          },
        )
      case S.SPAWN_ENTITY =>
        val sp = WrapperPlayServerSpawnEntity(event)
        val id = sp.getEntityId
        if id >= 0 then
          val uuid = sp.getUUID.orElse(UUID(0L, id.toLong))
          val tpe = sp.getEntityType
          val pos = sp.getPosition
          player.latencyUtils.addRealTimeTask(
            player.spawnTransaction(id),
            () => {
              player.compensatedEntities.addEntity(id, uuid, tpe, pos.getX, pos.getY, pos.getZ)
              if tpe == EntityTypes.END_CRYSTAL then
                player.crystalTracker.onCrystalSpawn(id, player.tracking.tickIndex)
            },
          )
      case S.SPAWN_LIVING_ENTITY =>
        val sp = WrapperPlayServerSpawnLivingEntity(event)
        val id = sp.getEntityId
        val uuid = sp.getEntityUUID
        val tpe = sp.getEntityType
        val pos = sp.getPosition
        player.latencyUtils.addRealTimeTask(
          player.spawnTransaction(id),
          () => player.compensatedEntities.addEntity(id, uuid, tpe, pos.getX, pos.getY, pos.getZ),
        )
      case S.SPAWN_PAINTING =>
        val sp = WrapperPlayServerSpawnPainting(event)
        val id = sp.getEntityId
        val uuid = sp.getUUID
        player.latencyUtils.addRealTimeTask(
          player.spawnTransaction(id),
          () => player.compensatedEntities.addEntity(id, uuid, EntityTypes.PAINTING),
        )
      case S.SPAWN_PLAYER =>
        val sp = WrapperPlayServerSpawnPlayer(event)
        val id = sp.getEntityId
        val uuid = sp.getUUID
        val pos = sp.getPosition
        player.latencyUtils.addRealTimeTask(
          player.spawnTransaction(id),
          () => player.compensatedEntities.addEntity(id, uuid, EntityTypes.PLAYER, pos.getX, pos.getY, pos.getZ),
        )
      case S.DESTROY_ENTITIES =>
        val ids = WrapperPlayServerDestroyEntities(event).getEntityIds
        ids.foreach(id => player.transactions.noteDespawned(id))
        val destroyTrans = player.lastSentTransaction()
        player.latencyUtils.addRealTimeTask(
          destroyTrans,
          () => {
            val self = player.compensatedEntities.self
            ids.foreach: id =>
              player.crystalTracker.onEntityRemoved(id)
              val ent = player.compensatedEntities.getEntity(id)
              if ent != null then
                ent.isDead = true
                if self.riding eq ent then self.eject()
          },
        )
        player.latencyUtils.addRealTimeTask(
          destroyTrans + 1,
          () => {
            ids.foreach: id =>
              player.compensatedEntities.removeEntity(id)
              player.compensatedFireworks.removeFirework(id)
          },
        )
      case S.ENTITY_STATUS =>
        val st = WrapperPlayServerEntityStatus(event)
        if st.getStatus == 3 then
          val id = st.getEntityId
          player.latencyUtils.addRealTimeTask(
            player.bracketTransaction(event),
            () => {
              val ent = player.compensatedEntities.getEntity(id)
              if ent != null then ent.isDead = true
            },
          )
      case S.UPDATE_ATTRIBUTES =>
        val attrs = WrapperPlayServerUpdateAttributes(event)
        val id = attrs.getEntityId
        val props = attrs.getProperties
        val trans = if id == player.entityId then player.bracketTransaction(event) else player.lastSentTransaction()
        player.latencyUtils.addRealTimeTask(trans, () => player.compensatedEntities.updateAttributes(id, props))
      case S.ENTITY_METADATA =>
        onMetadata(event, player)
      case S.ENTITY_RELATIVE_MOVE =>
        val m = WrapperPlayServerEntityRelativeMove(event)
        moveEntity(player, m.getEntityId, Vector3d(m.getDeltaX, m.getDeltaY, m.getDeltaZ), relative = true, hasPos = true)
      case S.ENTITY_RELATIVE_MOVE_AND_ROTATION =>
        val m = WrapperPlayServerEntityRelativeMoveAndRotation(event)
        moveEntity(player, m.getEntityId, Vector3d(m.getDeltaX, m.getDeltaY, m.getDeltaZ), relative = true, hasPos = true)
      case S.ENTITY_TELEPORT =>
        val tp = WrapperPlayServerEntityTeleport(event)
        moveEntity(player, tp.getEntityId, tp.getPosition, relative = false, hasPos = true)
      case S.ENTITY_ROTATION =>
        val r = WrapperPlayServerEntityRotation(event)
        moveEntity(player, r.getEntityId, Vector3d(0.0, 0.0, 0.0), relative = true, hasPos = false)
      case S.ENTITY_POSITION_SYNC =>
        val sync = WrapperPlayServerEntityPositionSync(event)
        moveEntity(player, sync.getId, sync.getValues.getPosition, relative = false, hasPos = true)
      case S.SET_PASSENGERS =>
        val w = WrapperPlayServerSetPassengers(event)
        val vehicleId = w.getEntityId
        val isPassenger = w.getPassengers.contains(player.entityId)
        player.latencyUtils.addRealTimeTask(
          player.bracketTransaction(event),
          () => {
            val self = player.compensatedEntities.self
            val vehicle = player.compensatedEntities.getEntity(vehicleId)
            if vehicle != null then
              if isPassenger then
                self.mount(vehicle)
                player.tracking.inVehicle = true
              else if self.riding eq vehicle then
                self.eject()
                player.tracking.inVehicle = self.riding != null
          },
        )
      case S.ATTACH_ENTITY =>
        val w = WrapperPlayServerAttachEntity(event)
        if !w.isLeash && w.getAttachedId == player.entityId then
          val holdingId = w.getHoldingId
          player.latencyUtils.addRealTimeTask(
            player.bracketTransaction(event),
            () => {
              val self = player.compensatedEntities.self
              if holdingId != -1 then
                val vehicle = player.compensatedEntities.getEntity(holdingId)
                if vehicle != null then
                  self.mount(vehicle)
                  player.tracking.inVehicle = true
              else if self.riding != null then
                self.eject()
                player.tracking.inVehicle = self.riding != null
            },
          )
      case S.CHUNK_DATA =>
        val col = WrapperPlayServerChunkData(event).getColumn
        val sections = col.getChunks
        if sections != null then player.compensatedWorld.onChunkLoad(col.getX, col.getZ, sections)
      case S.UNLOAD_CHUNK =>
        val u = WrapperPlayServerUnloadChunk(event)
        player.compensatedWorld.onChunkUnload(u.getChunkX, u.getChunkZ)
      case S.BLOCK_CHANGE =>
        val bc = WrapperPlayServerBlockChange(event)
        val pos = bc.getBlockPosition
        val st = bc.getBlockState
        player.latencyUtils.addRealTimeTask(
          player.lastSentTransaction(),
          () => player.compensatedWorld.setBlock(pos.getX, pos.getY, pos.getZ, st),
        )
      case S.MULTI_BLOCK_CHANGE =>
        val mb = WrapperPlayServerMultiBlockChange(event)
        val ver = PacketEvents.getAPI.getServerManager.getVersion.toClientVersion
        val changes = mb.getBlocks.map(b => (b.getX, b.getY, b.getZ, b.getBlockState(ver)))
        player.latencyUtils.addRealTimeTask(
          player.lastSentTransaction(),
          () => changes.foreach((x, y, z, st) => player.compensatedWorld.setBlock(x, y, z, st)),
        )
      case S.SET_SLOT =>
        val w = WrapperPlayServerSetSlot(event)
        if w.getWindowId == 0 then
          val tracking = player.tracking
          val hotbarIdx = w.getSlot - 36
          val itemType = if w.getItem != null then w.getItem.getType else null
          if w.getSlot == 45 then
            tracking.offhandItem = itemType
            tracking.updateActiveItem()
          else if hotbarIdx >= 0 && hotbarIdx < ShardTrackingState.HOTBAR_SIZE then
            tracking.hotbarItems(hotbarIdx) = itemType
            tracking.updateActiveItem()
            refreshAttackSpeed(player)
      case S.WINDOW_ITEMS =>
        val w = WrapperPlayServerWindowItems(event)
        if w.getWindowId == 0 then
          val items = w.getItems
          val tracking = player.tracking
          var i = 0
          while i < ShardTrackingState.HOTBAR_SIZE do
            val slot = 36 + i
            if slot < items.size() then
              val st = items.get(slot)
              tracking.hotbarItems(i) = if st != null then st.getType else null
            i += 1
          if 45 < items.size() then
            val st = items.get(45)
            tracking.offhandItem = if st != null then st.getType else null
          tracking.updateActiveItem()
          refreshAttackSpeed(player)
      case S.PLAYER_POSITION_AND_LOOK =>
        val w = WrapperPlayServerPlayerPositionAndLook(event)
        val transId = player.bracketTransaction(event)
        player.pendingTeleports.add(
          ShardPlayer.TeleportData(Vector3d(w.getX, w.getY, w.getZ), w.getYaw, w.getPitch, w.getRelativeFlags, transId)
        )
      case S.PLAYER_ROTATION =>
        val w = WrapperPlayServerPlayerRotation(event)
        val transId = player.preTransaction()
        val storedPitch =
          if w.isRelativePitch then w.getPitch
          else math.max(-90f, math.min(90f, w.getPitch % 360f))
        player.pendingRotations.add(
          ShardPlayer.RotationData(w.getYaw, storedPitch, w.isRelativeYaw, w.isRelativePitch, transId)
        )
      case _ => ()

  private def moveEntity(player: ShardPlayer, entityId: Int, delta: Vector3d, relative: Boolean, hasPos: Boolean): Unit =
    val entity = player.compensatedEntities.getEntity(entityId)
    if entity != null then
      if entity.lastTransactionHung == player.lastSentTransaction() then player.sendTransaction()
      entity.lastTransactionHung = player.lastSentTransaction()
    val lastTrans = player.lastSentTransaction()
    player.latencyUtils.addRealTimeTask(
      lastTrans,
      () => {
        val ent = player.compensatedEntities.getEntity(entityId)
        if ent != null then ent.onFirstTransaction(relative, hasPos, delta.getX, delta.getY, delta.getZ, player)
      },
    )
    player.latencyUtils.addRealTimeTask(
      lastTrans + 1,
      () => {
        val ent = player.compensatedEntities.getEntity(entityId)
        if ent != null then ent.onSecondTransaction()
      },
    )

  private val ageableMobs: Set[EntityType] = Set(
    EntityTypes.ZOMBIE, EntityTypes.ZOMBIE_VILLAGER, EntityTypes.HUSK, EntityTypes.DROWNED,
    EntityTypes.PIGLIN, EntityTypes.ZOGLIN, EntityTypes.PIG, EntityTypes.COW,
    EntityTypes.CHICKEN, EntityTypes.SHEEP, EntityTypes.VILLAGER, EntityTypes.HORSE,
    EntityTypes.DONKEY, EntityTypes.MULE, EntityTypes.LLAMA, EntityTypes.MOOSHROOM,
    EntityTypes.WOLF, EntityTypes.CAT, EntityTypes.OCELOT, EntityTypes.RABBIT,
    EntityTypes.POLAR_BEAR, EntityTypes.TURTLE, EntityTypes.PANDA, EntityTypes.FOX,
    EntityTypes.BEE, EntityTypes.HOGLIN, EntityTypes.STRIDER, EntityTypes.GOAT,
    EntityTypes.AXOLOTL, EntityTypes.FROG, EntityTypes.CAMEL, EntityTypes.SNIFFER,
    EntityTypes.ARMADILLO,
  )

  private def onMetadata(event: PacketSendEvent, player: ShardPlayer): Unit =
    val meta = WrapperPlayServerEntityMetadata(event)
    val entityId = meta.getEntityId
    val isSelf = entityId == player.entityId
    val metadata = meta.getEntityMetadata
    val serverVersion = PacketEvents.getAPI.getServerManager.getVersion
    val livingFlagsIndex =
      if serverVersion.isNewerThanOrEquals(ServerVersion.V_1_17) then 8
      else if serverVersion.isNewerThanOrEquals(ServerVersion.V_1_14) then 7
      else if serverVersion.isNewerThanOrEquals(ServerVersion.V_1_10) then 6
      else 5

    player.latencyUtils.addRealTimeTask(
      player.lastSentTransaction(),
      () => {
        val iter = metadata.iterator()
        while iter.hasNext do
          val data: EntityData[?] = iter.next()
          if data.getIndex == 0 then
            data.getValue match
              case b: java.lang.Byte if isSelf =>
                val flags = b.intValue()
                player.tracking.gliding =
                  (flags & 0x80) != 0 && player.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_9)
                player.tracking.swimming =
                  (flags & 0x10) != 0 && player.clientVersion.isNewerThanOrEquals(ClientVersion.V_1_13)
              case _ => ()
          else if data.getIndex == livingFlagsIndex then
            data.getValue match
              case b: java.lang.Byte if isSelf =>
                val livingFlags = b.intValue()
                player.tracking.riptideActive = (livingFlags & 0x04) == 0x04
                player.tracking.isUsingItem = (livingFlags & 0x01) != 0
                player.tracking.usingOffhand = (livingFlags & 0x02) != 0
                player.tracking.updateActiveItem()
              case _ => ()
          else if !isSelf then
            val entity = player.compensatedEntities.getEntity(entityId)
            if entity != null then
              val v = data.getValue
              val tpe = entity.tpe
              v match
                case pose: EntityPose if tpe == EntityTypes.PLAYER =>
                  entity.pose = pose.ordinal
                case b: java.lang.Boolean if data.getIndex >= 15 && data.getIndex <= 17 && ageableMobs.contains(tpe) =>
                  entity.isBaby = b.booleanValue()
                case i: java.lang.Integer if data.getIndex >= 15 && (tpe == EntityTypes.SLIME || tpe == EntityTypes.MAGMA_CUBE) =>
                  entity.slimeSize = i.intValue()
                case i: java.lang.Integer if data.getIndex >= 15 && tpe == EntityTypes.PHANTOM =>
                  entity.phantomSize = i.intValue()
                case _ => ()
              if tpe == EntityTypes.FIREWORK_ROCKET then
                val offset =
                  if serverVersion.isOlderThanOrEquals(ServerVersion.V_1_12_2) then 2
                  else if serverVersion.isOlderThanOrEquals(ServerVersion.V_1_16_5) then 1
                  else 0
                if data.getIndex == 9 - offset then
                  val attachedId: Option[Int] = v match
                    case i: java.lang.Integer => Some(i.intValue())
                    case opt: Optional[?] =>
                      if opt.isPresent then
                        opt.get() match
                          case i: java.lang.Integer => Some(i.intValue())
                          case _ => None
                      else None
                    case _ => None
                  if attachedId.contains(player.entityId) then
                    player.compensatedFireworks.addNewFirework(entityId)
      },
    )

  private def refreshAttackSpeed(player: ShardPlayer): Unit =
    val tracking = player.tracking
    val item =
      if tracking.heldSlot >= 0 && tracking.heldSlot < tracking.hotbarItems.length then tracking.hotbarItems(tracking.heldSlot)
      else null
    tracking.attackSpeed = itemAttackSpeed(item)

  private def itemAttackSpeed(item: ItemType | Null): Float =
    if item == ItemTypes.WOODEN_SWORD || item == ItemTypes.STONE_SWORD || item == ItemTypes.IRON_SWORD ||
      item == ItemTypes.GOLDEN_SWORD || item == ItemTypes.DIAMOND_SWORD || item == ItemTypes.NETHERITE_SWORD
    then 1.6f
    else if item == ItemTypes.WOODEN_AXE || item == ItemTypes.STONE_AXE then 0.8f
    else if item == ItemTypes.IRON_AXE then 0.9f
    else if item == ItemTypes.GOLDEN_AXE || item == ItemTypes.DIAMOND_AXE || item == ItemTypes.NETHERITE_AXE then 1.0f
    else if item == ItemTypes.WOODEN_PICKAXE || item == ItemTypes.STONE_PICKAXE || item == ItemTypes.IRON_PICKAXE ||
      item == ItemTypes.GOLDEN_PICKAXE || item == ItemTypes.DIAMOND_PICKAXE || item == ItemTypes.NETHERITE_PICKAXE
    then 1.2f
    else if item == ItemTypes.WOODEN_SHOVEL || item == ItemTypes.STONE_SHOVEL || item == ItemTypes.IRON_SHOVEL ||
      item == ItemTypes.GOLDEN_SHOVEL || item == ItemTypes.DIAMOND_SHOVEL || item == ItemTypes.NETHERITE_SHOVEL
    then 1.0f
    else if item == ItemTypes.TRIDENT then 1.1f
    else 4.0f

  private def applyPotion(tracking: ShardTrackingState, potionType: PotionType | Null, amplifier: Int): Unit =
    if potionType == PotionTypes.JUMP_BOOST then tracking.jumpAmplifier = amplifier
    else if potionType == PotionTypes.SLOW_FALLING then tracking.slowFalling = true
    else if potionType == PotionTypes.BLINDNESS then tracking.hasBlindness = true
    else if potionType == PotionTypes.SPEED then tracking.speedAmplifier = amplifier
    else if potionType == PotionTypes.SLOWNESS then tracking.slownessAmplifier = amplifier
    else if potionType == PotionTypes.HASTE then tracking.hasteAmplifier = amplifier
    else if potionType == PotionTypes.MINING_FATIGUE then tracking.miningFatigueAmplifier = amplifier
    else if potionType == PotionTypes.LEVITATION then tracking.levitationAmplifier = amplifier
    else if potionType == PotionTypes.DOLPHINS_GRACE then tracking.dolphinsGrace = true

  private def clearPotion(tracking: ShardTrackingState, potionType: PotionType | Null): Unit =
    if potionType == PotionTypes.JUMP_BOOST then tracking.jumpAmplifier = -1
    else if potionType == PotionTypes.SLOW_FALLING then tracking.slowFalling = false
    else if potionType == PotionTypes.BLINDNESS then tracking.hasBlindness = false
    else if potionType == PotionTypes.SPEED then tracking.speedAmplifier = -1
    else if potionType == PotionTypes.SLOWNESS then tracking.slownessAmplifier = -1
    else if potionType == PotionTypes.HASTE then tracking.hasteAmplifier = -1
    else if potionType == PotionTypes.MINING_FATIGUE then tracking.miningFatigueAmplifier = -1
    else if potionType == PotionTypes.LEVITATION then tracking.levitationAmplifier = -1
    else if potionType == PotionTypes.DOLPHINS_GRACE then tracking.dolphinsGrace = false
