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

package eigen.paper.net

import scala.reflect.ClassTag

import com.github.retrooper.packetevents.PacketEvents
import com.github.retrooper.packetevents.event.{PacketReceiveEvent, PacketSendEvent}
import com.github.retrooper.packetevents.manager.server.ServerVersion
import com.github.retrooper.packetevents.protocol.packettype.{PacketType, PacketTypeCommon}
import com.github.retrooper.packetevents.protocol.player.User
import com.github.retrooper.packetevents.wrapper.play.client.{WrapperPlayClientPong, WrapperPlayClientWindowConfirmation}
import com.github.retrooper.packetevents.wrapper.play.server.{WrapperPlayServerPing, WrapperPlayServerWindowConfirmation}

/**
 * Version-specialized protocol and transaction codec resolved once for the server process, plus
 * O(1) ordinal-indexed packet dispatch tables for `PacketType.Play`.
 *
 * Why this exists:
 *   - Scala 3 pattern matching on Java enums (`PacketType.Play.Client` / `Server`) compiles to a
 *     linear `if (tpe == X)` chain rather than a bytecode `tableswitch` (to preserve binary
 *     compatibility across enum edits). Indexing a dense array by `Enum.ordinal` turns every
 *     packet receive/send into a single bounds-checked array read + monomorphic/bimorphic call.
 *   - Server-version differences (`1.17+` `PING`/`PONG` vs `< 1.17` window-0 `WINDOW_CONFIRMATION`,
 *     living-entity metadata index, etc.) are bound once into a sealed [[ServerProtocol]]
 *     singleton so the hot packet path executes zero version branches.
 */
sealed trait ServerProtocol:
  /** Index of the living-entity flags byte in `ENTITY_METADATA`. */
  val livingFlagsIndex: Int

  /** Outbound packet type carrying server → client transactions on this server version. */
  val outboundTransactionType: PacketType.Play.Server

  /** Inbound packet type carrying client → server transaction answers on this server version. */
  val inboundTransactionType: PacketType.Play.Client

  /** Sends one transaction with wire `id` to `user`. */
  def sendTransaction(user: User, id: Short): Unit

  /** Extracts the wire transaction id from an outbound transaction packet, or `0` if unrelated. */
  def readSentId(event: PacketSendEvent): Int

  /** Extracts the wire transaction id from an inbound transaction answer, or `0` if unrelated. */
  def readAckId(event: PacketReceiveEvent): Int

object ServerProtocol:

  /** 1.17+ servers: `PLAY/PING` and `PLAY/PONG` with 32-bit id (we use positive 15-bit shorts). */
  case object ModernPing extends ServerProtocol:
    val livingFlagsIndex: Int = 8
    val outboundTransactionType: PacketType.Play.Server = PacketType.Play.Server.PING
    val inboundTransactionType: PacketType.Play.Client = PacketType.Play.Client.PONG

    def sendTransaction(user: User, id: Short): Unit =
      user.sendPacket(WrapperPlayServerPing(id.toInt))

    def readSentId(event: PacketSendEvent): Int =
      WrapperPlayServerPing(event).getId

    def readAckId(event: PacketReceiveEvent): Int =
      WrapperPlayClientPong(event).getId

  /** `< 1.17` servers: window-0 `WINDOW_CONFIRMATION` (`accepted = false` on send). */
  final class LegacyWindowConfirmation(val livingFlagsIndex: Int) extends ServerProtocol:
    val outboundTransactionType: PacketType.Play.Server = PacketType.Play.Server.WINDOW_CONFIRMATION
    val inboundTransactionType: PacketType.Play.Client = PacketType.Play.Client.WINDOW_CONFIRMATION

    def sendTransaction(user: User, id: Short): Unit =
      user.sendPacket(WrapperPlayServerWindowConfirmation(0, id, false))

    def readSentId(event: PacketSendEvent): Int =
      val w = WrapperPlayServerWindowConfirmation(event)
      if w.getWindowId == 0 && !w.isAccepted then w.getActionId.toInt else 0

    def readAckId(event: PacketReceiveEvent): Int =
      val w = WrapperPlayClientWindowConfirmation(event)
      if w.getWindowId == 0 then w.getActionId.toInt else 0

  /** Resolves the protocol strategy for a given `ServerVersion`. */
  def forVersion(server: ServerVersion): ServerProtocol =
    if server.isNewerThanOrEquals(ServerVersion.V_1_17) then ModernPing
    else
      val livingIdx =
        if server.isNewerThanOrEquals(ServerVersion.V_1_14) then 7
        else if server.isNewerThanOrEquals(ServerVersion.V_1_10) then 6
        else 5
      LegacyWindowConfirmation(livingIdx)

  /** Process-wide protocol strategy resolved from PacketEvents (defaults to [[ModernPing]] in unit tests). */
  lazy val current: ServerProtocol =
    try forVersion(PacketEvents.getAPI.getServerManager.getVersion)
    catch case _: Throwable => ModernPing

/**
 * Dense O(1) dispatch table indexed by `Enum.ordinal`, replacing linear `match` chains over
 * `PacketType.Play.Client` and `PacketType.Play.Server` with a single array lookup.
 */
final class PacketTable[K <: Enum[K], H >: Null <: AnyRef](
    private val handlers: Array[H | Null],
    private val runtimeClass: Class[K],
):
  /** Returns the handler registered for `tpe`, or `null` if `tpe` is not of `K` or unhandled. */
  inline def get(tpe: PacketTypeCommon): H | Null =
    if runtimeClass.isInstance(tpe) then
      val ord = tpe.asInstanceOf[K].ordinal
      if ord >= 0 && ord < handlers.length then handlers(ord) else null
    else null

object PacketTable:
  def build[K <: Enum[K]: ClassTag, H >: Null <: AnyRef: ClassTag](
      universe: Array[K]
  )(
      entries: (K, H)*
  ): PacketTable[K, H] =
    val cls = summon[ClassTag[K]].runtimeClass.asInstanceOf[Class[K]]
    val table = new Array[H | Null](universe.length)
    var i = 0
    while i < entries.length do
      val (k, h) = entries(i)
      table(k.ordinal) = h
      i += 1
    PacketTable(table, cls)
