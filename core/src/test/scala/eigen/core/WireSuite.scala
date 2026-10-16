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

package eigen.core

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.util.HexFormat

import eigen.core.history.{Window, WindowKind}
import eigen.core.schema.{Manifest, Schema}
import eigen.core.wire.{ByteSink, FrameCodec, MessageType, Messages}

/**
 * Byte-level checks of the wire format, plus golden fixtures shared with the backend: the
 * backend's test suite decodes the hex files in `docs/fixtures` and must agree with what is asserted here.
 * Regenerate them with `EIGEN_WRITE_FIXTURES=1 sbt core/testFull` after a deliberate change.
 */
class WireSuite extends munit.FunSuite:

  private def window(kind: WindowKind, layoutCombat: Boolean, rows: Int, seed: Int): Window =
    val layout = if layoutCombat then Schema.Combat else Schema.Calm
    val data = Array.tabulate(layout.cols.length, rows): (c, r) =>
      layout.cols(c).tpe match
        case eigen.core.schema.ColType.F32 => (seed + c) * 0.25f + r
        case eigen.core.schema.ColType.I8 => ((c + r) % 3 - 1).toFloat
        case _ => ((seed + c + r) % 2).toFloat
    val w = Window(kind, layout, epoch = 2, engagement = if kind == WindowKind.Calm then 0 else 1, firstTick = 1000 + seed, rows, anchor = if kind == WindowKind.Calm then -1 else 1, data)
    w.handle = 7
    w.session = 3
    w.purpose = Window.PurposeCollect
    w.id = seed
    w

  private val sample = Seq(window(WindowKind.Attack, layoutCombat = true, rows = 3, seed = 1), window(WindowKind.Calm, layoutCombat = false, rows = 2, seed = 2))

  test("the manifest hash is the CRC-32 of the canonical JSON"):
    val crc = java.util.zip.CRC32()
    crc.update(Manifest.json.getBytes(UTF_8))
    assertEquals(Manifest.hash, f"${crc.getValue}%08x")
    assert(!Manifest.json.contains(' '), "canonical JSON has no whitespace")
    assert(Manifest.json.startsWith("{\"version\":1,\"layouts\":[{\"id\":1,\"name\":\"combat\""))

  test("a WINDOWS frame has the documented header and exact size"):
    val frame = FrameCodec(compressAbove = Int.MaxValue).encodeWindows(41, sample)
    assertEquals(frame(0) & 0xff, MessageType.Windows)
    assertEquals(frame(1).toInt, 0)
    assertEquals(ByteSink.readU32(frame, 4), 41L)
    assertEquals(ByteSink.readU16(frame, 8), 2)
    val expected = FrameCodec.HeaderBytes + 2 + sample.map(_.wireBytes).sum
    assertEquals(frame.length, expected)
    // First window header, right after the u16 count.
    val w = 10
    assertEquals(ByteSink.readU32(frame, w), 7L) // handle
    assertEquals(ByteSink.readU32(frame, w + 4), 1L) // window id
    assertEquals(ByteSink.readU32(frame, w + 8), 3L) // session
    assertEquals(frame(w + 12) & 0xff, WindowKind.Attack.code)
    assertEquals(frame(w + 13) & 0xff, Window.PurposeCollect)
    assertEquals(frame(w + 14) & 0xff, Schema.Combat.id)
    assertEquals(ByteSink.readU32(frame, w + 16), 2L) // epoch
    assertEquals(ByteSink.readU32(frame, w + 20), 1L) // engagement
    assertEquals(ByteSink.readU32(frame, w + 24), 1001L) // first tick
    assertEquals(ByteSink.readU16(frame, w + 28), 3) // rows
    assertEquals(ByteSink.readI16(frame, w + 30), 1) // anchor
    // First column (dots_yaw, f32) of the first window, row 0.
    assertEquals(ByteSink.readF32(frame, w + Window.HeaderBytes), 0.25f)

  test("compressed frames decode back to the same payload"):
    val plain = FrameCodec(compressAbove = Int.MaxValue).encodeWindows(5, sample)
    val packed = FrameCodec(compressAbove = 0).encodeWindows(5, sample)
    assertEquals(packed(1).toInt, 1)
    val decoded = FrameCodec.decode(packed, 1 << 20).toOption.get
    assertEquals(decoded.payload.toSeq, plain.drop(FrameCodec.HeaderBytes).toSeq)
    assert(FrameCodec.decode(packed, 16).isLeft, "size cap applies to the inflated payload")

  test("HELLO embeds the manifest verbatim"):
    val hello = String(Messages.hello("00112233aabbccdd", "eigen test", "Paper test"), UTF_8)
    assert(hello.contains("\"schema\":" + Manifest.json + ","))
    assert(hello.contains("\"schema_hash\":\"" + Manifest.hash + "\""))

  test("golden fixtures match the encoder"):
    val fixtures = Seq(
      "windows_v1.hex" -> FrameCodec(compressAbove = Int.MaxValue).encodeWindows(41, sample),
      "windows_v1_deflate.hex" -> FrameCodec(compressAbove = 0).encodeWindows(42, sample),
      "hello_v1.hex" -> FrameCodec().encode(MessageType.Hello, 0, Messages.hello("00112233aabbccdd", "eigen test", "Paper test")),
    )
    val dir = fixtureDir
    for (name, bytes) <- fixtures do
      val file = dir.resolve(name)
      val hex = HexFormat.of().formatHex(bytes)
      if sys.env.contains("EIGEN_WRITE_FIXTURES") then Files.writeString(file, hex + "\n")
      else assertEquals(Files.readString(file).trim, hex, s"$name is stale; regenerate it")

  private def fixtureDir: Path =
    var dir = Paths.get("").toAbsolutePath
    while dir != null && !Files.isDirectory(dir.resolve("docs")) do dir = dir.getParent
    val out = dir.resolve("docs/fixtures")
    Files.createDirectories(out)
    out
