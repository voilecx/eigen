package eigen.core

object Angles:

  /** Wraps an angle in degrees into [-180, 180). */
  inline def wrapDegrees(deg: Float): Float =
    val r = deg % 360f
    if r >= 180f then r - 360f
    else if r < -180f then r + 360f
    else r

  /** Signed yaw change between two consecutive rotations, taking the shortest way around. */
  inline def yawDelta(from: Float, to: Float): Float = wrapDegrees(to - from)
