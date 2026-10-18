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

import eigen.core.schema.Schema
import eigen.core.wire.ByteSink

/**
 * Binary snapshot of a player's per-session baseline state (mouse sensitivity estimator,
 * engagement counter, and in-progress calm track) for cross-instance handoff via Garnet.
 */
object Handoff:
  private val Magic = 0x4547484f // "EGHO"
  private val Version = 1

  def encode(sim: PlayerSim): Array[Byte] =
    val sink = ByteSink(512)
    encodeTo(sim, sink)
    sink.toArray

  def encodeTo(sim: PlayerSim, sink: ByteSink): Unit =
    sink.u32(Magic.toLong & 0xffffffffL)
    sink.u8(Version)
    sink.u8(Schema.Version)
    sink.u8(Schema.Calm.cols.size)
    sink.u32(sim.combat.engagement.toLong & 0xffffffffL)
    sim.rotation.yawAxis.writeTo(sink)
    sim.rotation.pitchAxis.writeTo(sink)
    sim.calm.writeTo(sink)

  def restore(sim: PlayerSim, bytes: Array[Byte]): Boolean =
    if bytes == null || bytes.length < 11 then return false
    if ByteSink.readI32(bytes, 0) != Magic then return false
    if (bytes(4) & 0xff) != Version then return false
    if (bytes(5) & 0xff) != Schema.Version then return false
    if (bytes(6) & 0xff) != Schema.Calm.cols.size then return false
    val savedEngagement = ByteSink.readI32(bytes, 7)
    if savedEngagement < 0 then return false
    // Do not overwrite if local calibration has already converged on the new server
    if sim.rotation.yawAxis.converged && sim.rotation.pitchAxis.converged then return false
    val afterYaw = sim.rotation.yawAxis.readFrom(bytes, 11)
    if afterYaw < 0 then
      sim.rotation.yawAxis.reset()
      return false
    val afterPitch = sim.rotation.pitchAxis.readFrom(bytes, afterYaw)
    if afterPitch < 0 then
      sim.rotation.yawAxis.reset()
      sim.rotation.pitchAxis.reset()
      return false
    val afterCalm = sim.calm.readFrom(bytes, afterPitch)
    if afterCalm != bytes.length then
      sim.calm.flush()
      return false
    if savedEngagement > sim.combat.engagement then
      sim.combat.engagement = savedEngagement
    true
