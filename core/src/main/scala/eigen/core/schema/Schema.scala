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

package eigen.core.schema

import scala.collection.immutable.ArraySeq

/** Wire type of a column. In memory every column is a `Float`; the type only decides encoding. */
enum ColType(val size: Int, val wire: String):
  case U8 extends ColType(1, "u8")
  case I8 extends ColType(1, "i8")
  case U16 extends ColType(2, "u16")
  case F32 extends ColType(4, "f32")

/** Logical stream a column belongs to; `Mask` and `Meta` are never model inputs. */
enum Stream(val wire: String):
  case Rot extends Stream("rot")
  case Inp extends Stream("inp")
  case Tgt extends Stream("tgt")
  case Mask extends Stream("mask")
  case Meta extends Stream("meta")

/** A subset of columns sent together; see the manifest in `docs/protocol.md` §4. */
final class Layout(val id: Int, val name: String, val cols: ArraySeq[Schema.Col]):
  /** Bytes one row of this layout occupies on the wire. */
  val rowBytes: Int = cols.iterator.map(_.tpe.size).sum

/**
 * The per-tick feature frame. Order here is the wire order; append new columns at the end of
 * their layout and bump [[Schema.Version]].
 */
object Schema:
  import ColType.*
  import Stream.*

  inline val Version = 1

  /** A column of the per-tick frame: a zero-cost index into the frame's column arrays. */
  opaque type Col = Int

  object Col:
    extension (c: Col)
      inline def index: Int = c
      def name: String = names(c)
      def tpe: ColType = types(c)
      def stream: Stream = streams(c)

    /** The column with the given index; for codecs that walk columns generically. */
    def at(index: Int): Col =
      require(index >= 0 && index < width, s"no column $index")
      index

  private val nameBuf = Array.newBuilder[String]
  private val typeBuf = Array.newBuilder[ColType]
  private val streamBuf = Array.newBuilder[Stream]
  private var next = 0

  private def col(name: String, tpe: ColType, stream: Stream): Col =
    nameBuf += name; typeBuf += tpe; streamBuf += stream
    next += 1
    next - 1

  // Mouse: sensitivity-normalised counts and the residual from whole counts.
  val DotsYaw: Col = col("dots_yaw", F32, Rot)
  val DotsPitch: Col = col("dots_pitch", F32, Rot)
  val ResidYaw: Col = col("resid_yaw", F32, Rot)
  val ResidPitch: Col = col("resid_pitch", F32, Rot)

  // Keys and clicks.
  val Strafe: Col = col("strafe", I8, Inp)
  val Forward: Col = col("forward", I8, Inp)
  val Jump: Col = col("jump", U8, Inp)
  val Sprint: Col = col("sprint", U8, Inp)
  val Sneak: Col = col("sneak", U8, Inp)
  val Swing: Col = col("swing", U8, Inp)
  val Use: Col = col("use", U8, Inp)

  // Target geometry, in the player's view frame.
  val AimYaw: Col = col("aim_yaw", F32, Tgt)
  val AimPitch: Col = col("aim_pitch", F32, Tgt)
  val AimAngle: Col = col("aim_angle", F32, Tgt)
  val Distance: Col = col("distance", F32, Tgt)
  val RelFwd: Col = col("rel_fwd", F32, Tgt)
  val RelSide: Col = col("rel_side", F32, Tgt)
  val RelUp: Col = col("rel_up", F32, Tgt)
  val Attack: Col = col("attack", U8, Tgt)
  val Hit: Col = col("hit", U8, Tgt)
  val HitX: Col = col("hit_x", F32, Tgt)
  val HitY: Col = col("hit_y", F32, Tgt)
  val HitZ: Col = col("hit_z", F32, Tgt)
  val Miss: Col = col("miss", F32, Tgt)
  val Cooldown: Col = col("cooldown", F32, Tgt)
  val Crit: Col = col("crit", U8, Tgt)
  val SprintHit: Col = col("sprint_hit", U8, Tgt)
  val SameTarget: Col = col("same_target", U8, Tgt)

  // Validity of each model stream for this tick (1 = valid).
  val MaskRot: Col = col("m_rot", U8, Mask)
  val MaskInp: Col = col("m_inp", U8, Mask)
  val MaskTgt: Col = col("m_tgt", U8, Mask)

  // Context for balancing and audits. Never a model input.
  val Reasons: Col = col("reasons", U16, Meta)
  val Gate: Col = col("gate", U8, Meta)
  val Activity: Col = col("activity", U8, Meta)
  val Inflation: Col = col("inflation", F32, Meta)
  val TargetKind: Col = col("target_kind", U8, Meta)
  val ItemKind: Col = col("item_kind", U8, Meta)
  val PingBucket: Col = col("ping_bucket", U8, Meta)

  val names: Array[String] = nameBuf.result()
  val types: Array[ColType] = typeBuf.result()
  val streams: Array[Stream] = streamBuf.result()

  /** Number of columns in a full frame. */
  val width: Int = names.length

  val all: ArraySeq[Col] = ArraySeq.range(0, width)

  val Combat: Layout = Layout(1, "combat", all)

  val Calm: Layout = Layout(
    2,
    "calm",
    ArraySeq(
      DotsYaw, DotsPitch, ResidYaw, ResidPitch,
      Strafe, Forward, Jump, Sprint, Sneak, Swing, Use,
      MaskRot, MaskInp,
      Reasons, Activity, ItemKind,
    ),
  )

  val layouts: ArraySeq[Layout] = ArraySeq(Combat, Calm)
