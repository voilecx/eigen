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

import java.nio.ByteBuffer
import java.util.{HexFormat, UUID}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.CollectionConverters.*

import com.github.retrooper.packetevents.protocol.player.User
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import eigen.core.history.Window
import eigen.core.sim.{Handoff, WindowSystem}
import eigen.core.wire.Messages
import eigen.paper.collect.LocalSink
import eigen.paper.config.Settings
import eigen.paper.garnet.GarnetBridge
import eigen.paper.stream.StreamClient

/**
 * Registry of connected players and the router for the windows their simulations produce.
 * Lookups are concurrent; each [[EigenPlayer]] is otherwise owned by its network thread.
 */
final class Players(settings: Settings, stream: StreamClient, local: LocalSink, garnet: GarnetBridge):
  private val byUser = ConcurrentHashMap[User, EigenPlayer]()
  private val byUuid = ConcurrentHashMap[UUID, EigenPlayer]()
  private val byHandle = ConcurrentHashMap[Int, EigenPlayer]()
  private val pendingBrands = ConcurrentHashMap[User, String]()
  private val handles = AtomicInteger(0)
  private val macSpec = SecretKeySpec(settings.salt, "HmacSHA256")

  @volatile var onAnnounced: EigenPlayer => Unit = _ => ()

  def get(user: User): EigenPlayer | Null = byUser.get(user)
  def get(uuid: UUID): EigenPlayer | Null = byUuid.get(uuid)
  def byHandleOrNull(handle: Int): EigenPlayer | Null = byHandle.get(handle)
  def all: Iterable[EigenPlayer] = byUser.values().asScala

  /** Creates the player's state when its connection enters the game. */
  def connect(user: User): EigenPlayer =
    val p = byUser.computeIfAbsent(user, newPlayer)
    // The name is known now; announcing here (not only on the login event) makes it reliable.
    announce(p, user.getName)
    p

  private def newPlayer(user: User): EigenPlayer =
    val p = EigenPlayer(user, user.getUUID, handles.incrementAndGet(), Profiles.of(user.getClientVersion), pseudonymOf(user.getUUID), settings.sim, route)
    val brand = pendingBrands.remove(user)
    if brand != null then p.brand = brand
    p.exempt = settings.exemptBedrock && isBedrock(user.getUUID)
    byUuid.put(p.uuid, p)
    byHandle.put(p.handle, p)
    p

  /** Announces the player to the backend once its name and brand are known. */
  def announce(p: EigenPlayer, name: String): Unit =
    p.name = name
    if !p.announced then
      val pseudo = p.pseudonym
      stream.playerJoined(Messages.PlayerJoin(p.handle, pseudo, p.profile.protocol, p.brand))
      p.announced = true
      if !p.exempt && garnet.enabled then
        garnet.loadHandoff(pseudo): bytes =>
          p.post(() => { val _ = Handoff.restore(p.sim, bytes) })
      onAnnounced(p)

  def disconnect(user: User): EigenPlayer | Null =
    pendingBrands.remove(user)
    val p = byUser.remove(user)
    if p != null then
      // No further packets arrive, so the mailbox is drained here: every task a command
      // posted (a session start or stop) still runs on the connection's own thread.
      p.drain()
      if !p.exempt then
        if wantsWindows(p) && p.sim.calm.length >= settings.sim.calmMinTicks then
          WindowSystem.flushCalm(p.sim)
        if garnet.enabled then
          garnet.saveHandoff(p.pseudonym, Handoff.encode(p.sim))
      byUuid.remove(p.uuid, p)
      byHandle.remove(p.handle, p)
      if p.announced then stream.playerLeft(p.handle)
    p

  def rememberBrand(user: User, brand: String): Unit =
    val p = byUser.get(user)
    if p != null then p.brand = brand else pendingBrands.put(user, brand)

  /** Whether windows are wanted for this player right now (checked every tick). */
  def wantsWindows(p: EigenPlayer): Boolean =
    p.announced && !p.exempt && (p.session != null || (stream.enabled && stream.inference))

  /** Routes a finished window: to the backend for collection and/or inference, and to disk. */
  private def route(p: EigenPlayer, w: Window): Unit =
    val s = p.session
    val collect = s != null
    val infer = stream.enabled && stream.inference
    w.handle = p.handle
    if collect then
      w.session = s.nn.id
      s.nn.windows.incrementAndGet()
      if settings.sink.local then local.write(s.nn, w)
    val purpose =
      (if collect && settings.sink.server then Window.PurposeCollect else 0) |
        (if infer then Window.PurposeInfer else 0)
    if purpose != 0 then
      w.purpose = purpose
      stream.window(w)

  /** Stable pseudonymous id: HMAC-SHA256 of the UUID under this server's secret salt. */
  private def pseudonymOf(uuid: UUID): String =
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(macSpec)
    val bytes = ByteBuffer.allocate(16).putLong(uuid.getMostSignificantBits).putLong(uuid.getLeastSignificantBits).array()
    HexFormat.of().formatHex(mac.doFinal(bytes))

  /** Floodgate gives Bedrock players UUIDs with all-zero high bits. */
  private def isBedrock(uuid: UUID): Boolean = uuid.getMostSignificantBits == 0L