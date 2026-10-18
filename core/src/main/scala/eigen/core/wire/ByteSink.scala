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

package eigen.core.wire

import java.lang.invoke.{MethodHandles, VarHandle}
import java.nio.ByteOrder

/** Growable little-endian byte buffer. Reused across frames by the single transport thread. */
final class ByteSink(initial: Int = 64 * 1024):
  import ByteSink.*

  private var buf = new Array[Byte](initial)
  private var pos = 0

  def size: Int = pos
  def array: Array[Byte] = buf
  def reset(): Unit = pos = 0
  def toArray: Array[Byte] = java.util.Arrays.copyOf(buf, pos)

  def u8(v: Int): Unit =
    ensure(1)
    buf(pos) = v.toByte
    pos += 1

  def u16(v: Int): Unit =
    ensure(2)
    Short16.set(buf, pos, v.toShort)
    pos += 2

  def u32(v: Long): Unit =
    ensure(4)
    Int32.set(buf, pos, v.toInt)
    pos += 4

  def f32(v: Float): Unit =
    ensure(4)
    Float32.set(buf, pos, v)
    pos += 4

  def f64(v: Double): Unit =
    ensure(8)
    Double64.set(buf, pos, v)
    pos += 8

  def bytes(src: Array[Byte], off: Int, len: Int): Unit =
    ensure(len)
    System.arraycopy(src, off, buf, pos, len)
    pos += len

  /** Overwrites a u32 at an earlier offset (for back-patched counts). */
  def patchU16(at: Int, v: Int): Unit = Short16.set(buf, at, v.toShort)

  def ensure(n: Int): Unit =
    if pos + n > buf.length then buf = java.util.Arrays.copyOf(buf, math.max(buf.length * 2, pos + n))

object ByteSink:
  private val Short16: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Short]], ByteOrder.LITTLE_ENDIAN)
  private val Int32: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Int]], ByteOrder.LITTLE_ENDIAN)
  private val Float32: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Float]], ByteOrder.LITTLE_ENDIAN)
  private val Double64: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Double]], ByteOrder.LITTLE_ENDIAN)

  def readU16(a: Array[Byte], at: Int): Int = (Short16.get(a, at): Short) & 0xffff
  def readI16(a: Array[Byte], at: Int): Int = Short16.get(a, at): Short
  def readU32(a: Array[Byte], at: Int): Long = (Int32.get(a, at): Int) & 0xffffffffL
  def readI32(a: Array[Byte], at: Int): Int = Int32.get(a, at): Int
  def readF32(a: Array[Byte], at: Int): Float = Float32.get(a, at): Float
  def readF64(a: Array[Byte], at: Int): Double = Double64.get(a, at): Double
