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

package eigen.config

class ReadSuite extends munit.FunSuite:

  private case class Backend(enabled: Boolean, retries: Int, mode: String)

  private val backend: Read[Backend] = Read.section("backend"):
    for
      enabled <- Read.bool("enabled", default = false)
      retries <- Read.int("retries", default = 3, min = 0, max = 10)
      mode <- Read.choice("mode", "fast")("fast" -> "fast", "slow" -> "slow")
    yield Backend(enabled, retries, mode)

  private def read(entries: Map[String, Any]): Either[String, Backend] =
    backend.from(Source.of(entries))

  test("missing keys and sections fall back to their defaults"):
    assertEquals(read(Map.empty), Right(Backend(false, 3, "fast")))
    assertEquals(read(Map("backend" -> Map("retries" -> 5))), Right(Backend(false, 5, "fast")))

  test("present values are read with their types"):
    val doc = Map("backend" -> Map("enabled" -> true, "retries" -> 7, "mode" -> "slow"))
    assertEquals(read(doc), Right(Backend(true, 7, "slow")))

  test("errors name the dotted path of the offending key"):
    assertEquals(read(Map("backend" -> Map("retries" -> 11))), Left("backend.retries: must be between 0 and 10, got 11"))
    assertEquals(read(Map("backend" -> Map("enabled" -> "yes"))), Left("backend.enabled: must be true or false, got 'yes'"))
    assertEquals(read(Map("backend" -> Map("mode" -> "turbo"))), Left("backend.mode: must be one of fast, slow, got 'turbo'"))

  test("a section given as a scalar is an error, not silently defaulted"):
    assertEquals(read(Map("backend" -> 5)), Left("backend: must be a section"))

  test("numbers are accepted as whole numbers only when they are whole"):
    assertEquals(read(Map("backend" -> Map("retries" -> 2.0))), Right(Backend(false, 2, "fast")))
    assertEquals(read(Map("backend" -> Map("retries" -> 2.5))), Left("backend.retries: must be a whole number, got Double"))

  test("reads compose with map and flatMap"):
    val doubled = Read.int("n", default = 1).map(_ * 2)
    val chained = Read.int("n", default = 1).flatMap(n => Read.pure(n + 1))
    assertEquals(doubled.from(Source.of(Map("n" -> 4))), Right(8))
    assertEquals(chained.from(Source.of(Map.empty)), Right(2))

  test("ensure fails with the given key when a cross-key check does not hold"):
    val ordered: Read[(Int, Int)] = for
      low <- Read.int("low", 1)
      high <- Read.int("high", 2)
      _ <- Read.ensure(high >= low, "high", "must be at least low")
    yield (low, high)
    assertEquals(ordered.from(Source.of(Map("low" -> 5, "high" -> 3))), Left("high: must be at least low"))
    assertEquals(ordered.from(Source.of(Map("low" -> 1, "high" -> 3))), Right((1, 3)))
