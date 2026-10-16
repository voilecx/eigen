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

package eigen.core.aim

/**
 * Estimates one axis' rotation step — the angle a single mouse count turns the camera.
 *
 * The client turns by whole mouse counts times a sensitivity-dependent step, so consecutive
 * per-tick deltas share that step as a common divisor. We take the GCD of consecutive deltas
 * and keep the running mode of it. Once stable, `delta / step` gives raw mouse counts
 * ("dots"), which do not depend on the player's sensitivity, and the distance of dots from a
 * whole number exposes rotations that did not come from a mouse.
 */
final class AxisEstimator:
  import AxisEstimator.*

  private val modes = RunningMode(Window)
  private var lastAbs = 0.0
  private var step = 0.0
  private var stable = false

  // Recent residual quality, to notice a sensitivity change and start over.
  private val recentOff = new Array[Boolean](DriftWindow)
  private var recentHead = 0
  private var recentCount = 0
  private var offCount = 0

  def converged: Boolean = stable
  def currentStep: Double = step

  /**
   * Feeds the rotation of one tick. `ulp` is the float precision of the absolute angle, which
   * grows with |yaw| and limits how small a step can still be resolved.
   */
  def observe(delta: Double, ulp: Double): Unit =
    val abs = math.abs(delta)
    if abs <= 0 || abs >= MaxSampleDelta then return
    val divisor = gcd(abs, lastAbs)
    if divisor > MinDivisor && ulp < divisor * UlpFraction then
      if lastAbs != 0 then modes.add(divisor, math.max(BucketWidth, ulp * 2))
      lastAbs = abs
      if modes.size > Significant && modes.modeCount > Significant then
        step = modes.mode
        stable = true
    if stable then trackDrift(abs)

  /** Signed mouse counts for `delta`, or NaN until the step is known. */
  def dots(delta: Double): Double = if stable then delta / step else Double.NaN

  def reset(): Unit =
    modes.clear()
    lastAbs = 0; step = 0; stable = false
    recentHead = 0; recentCount = 0; offCount = 0

  private def trackDrift(abs: Double): Unit =
    val d = abs / step
    val off = math.abs(d - math.rint(d)) > DriftResidual
    if recentCount == DriftWindow then
      if recentOff(recentHead) then offCount -= 1
    else recentCount += 1
    recentOff(recentHead) = off
    if off then offCount += 1
    recentHead = (recentHead + 1) % DriftWindow
    if recentCount == DriftWindow && offCount * 2 > DriftWindow then reset()

object AxisEstimator:
  private inline val Window = 80
  private inline val Significant = 15
  private inline val MaxSampleDelta = 5.0
  private inline val BucketWidth = 1e-3
  private inline val UlpFraction = 1.0 / 16.0
  private inline val DriftWindow = 40
  private inline val DriftResidual = 0.2

  /** Smallest step the client can produce: sensitivity 0 → (0.2^3 * 8) * 0.15. */
  val MinDivisor: Double = math.pow(0.2, 3) * 8 * 0.15 - 1e-3

  /** Euclid on doubles, stopping at the smallest possible step. */
  def gcd(a0: Double, b0: Double): Double =
    if a0 == 0 then return 0
    var a = math.max(a0, b0)
    var b = math.min(a0, b0)
    while b > MinDivisor do
      val t = a - math.floor(a / b) * b
      a = b
      b = t
    a
