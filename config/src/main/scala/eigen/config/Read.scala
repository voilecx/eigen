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

/**
 * A typed read from a [[Source]]. Each key carries its default and its validation, so a schema is
 * a single `for` comprehension over reads:
 *
 * {{{
 * val backend = Read.section("backend"):
 *   for
 *     enabled <- Read.bool("enabled", default = false)
 *     retries <- Read.int("retries", default = 3, min = 0)
 *   yield Backend(enabled, retries)
 * }}}
 *
 * A missing key yields its default. The first problem stops the read, and its message names the
 * dotted path of the offending key, for example `backend.retries: must be at least 0`.
 */
final class Read[A] private (private[config] val run: Source => Either[String, A]):
  def map[B](f: A => B): Read[B] = new Read(source => run(source).map(f))

  def flatMap[B](f: A => Read[B]): Read[B] = new Read(source => run(source).flatMap(a => f(a).run(source)))

  /**
   * Applicative product of two independent reads: evaluates both against the same [[Source]] and
   * accumulates both error messages (separated by `"; "`) if both fail.
   */
  def zip[B](that: Read[B]): Read[(A, B)] = new Read(source =>
    (this.run(source), that.run(source)) match
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(e1), Left(e2)) => Left(s"$e1; $e2")
      case (Left(e1), _) => Left(e1)
      case (_, Left(e2)) => Left(e2)
  )

  /** Validates the parsed value under `key`, failing with `key: message` when `p` does not hold. */
  def filterOrElse(key: String, message: String)(p: A => Boolean): Read[A] =
    flatMap(a => if p(a) then Read.pure(a) else Read.fail(s"$key: $message"))

  /** Reads the whole document. */
  def from(source: Source): Either[String, A] = run(source)

object Read:
  def pure[A](value: A): Read[A] = new Read(_ => Right(value))

  def fail[A](message: String): Read[A] = new Read(_ => Left(message))

  /** Fails with `key: message` unless `condition` holds; for checks that relate several keys. */
  def ensure(condition: Boolean, key: String, message: String): Read[Unit] =
    if condition then pure(()) else fail(s"$key: $message")

  /** A scalar key with a default; `parse` validates and converts the raw value. */
  def key[A](name: String, default: A)(parse: Any => Either[String, A]): Read[A] =
    new Read(source =>
      source.value(name) match
        case None => Right(default)
        case Some(raw) => parse(raw).left.map(problem => s"$name: $problem"))

  def text(name: String, default: String): Read[String] =
    key(name, default) {
      case s: String => Right(s)
      case other => typeError(other, "text")
    }

  def bool(name: String, default: Boolean): Read[Boolean] =
    key(name, default) {
      case b: Boolean => Right(b)
      case other => typeError(other, "true or false")
    }

  def int(name: String, default: Int, min: Int = Int.MinValue, max: Int = Int.MaxValue): Read[Int] =
    key(name, default) {
      case n: Number if n.doubleValue % 1 == 0 =>
        val v = n.longValue
        if v < min || v > max then Left(s"must be between $min and $max, got $v") else Right(v.toInt)
      case other => typeError(other, "a whole number")
    }

  def double(name: String, default: Double): Read[Double] =
    key(name, default) {
      case n: Number => Right(n.doubleValue)
      case other => typeError(other, "a number")
    }

  /** One of `options`, written as text; `default` must be one of them. */
  def choice[A](name: String, default: String)(options: (String, A)*): Read[A] =
    val table = options.toMap
    key(name, table(default)) {
      case s: String => table.get(s.toLowerCase).toRight(s"must be one of ${options.map(_._1).mkString(", ")}, got '$s'")
      case other => typeError(other, "text")
    }

  /** One of the cases of a Scala 3 `enum`, matched case-insensitively by `productPrefix`. */
  def enumChoice[E <: scala.reflect.Enum](name: String, default: E, values: Array[E]): Read[E] =
    choice(name, default.productPrefix.toLowerCase)(values.map(v => v.productPrefix.toLowerCase -> v).toSeq*)

  /** A nested section. A missing section reads every key from its default; a scalar is an error. */
  def section[A](name: String)(inner: Read[A]): Read[A] =
    new Read(source =>
      val child = source.section(name)
      if child.isEmpty && source.value(name).isDefined then Left(s"$name: must be a section")
      else inner.run(child.getOrElse(Source.Empty)).left.map(problem => s"$name.$problem"))

  private def typeError(got: Any, expected: String): Left[String, Nothing] =
    Left(s"must be $expected, got ${describe(got)}")

  private def describe(value: Any): String = value match
    case s: String => s"'$s'"
    case other => other.getClass.getSimpleName
