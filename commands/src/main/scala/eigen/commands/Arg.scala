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

package eigen.commands

import scala.jdk.CollectionConverters.*

import com.mojang.brigadier.arguments.{ArgumentType, IntegerArgumentType, StringArgumentType}
import com.mojang.brigadier.context.CommandContext
import io.papermc.paper.command.brigadier.CommandSourceStack
import io.papermc.paper.command.brigadier.argument.ArgumentTypes
import io.papermc.paper.command.brigadier.argument.resolvers.selector.PlayerSelectorArgumentResolver
import org.bukkit.entity.Player

/**
 * A typed command argument: how Brigadier parses it, how to read the parsed value back, and
 * optional tab completions. Declare arguments once as values and read them with `ctx(arg)`.
 */
final class Arg[A] private (
    val name: String,
    private[commands] val brigadier: ArgumentType[?],
    private[commands] val read: (CommandContext[CommandSourceStack], String) => A,
    private[commands] val completions: Option[CommandSourceStack => Iterable[String]],
):
  /** Adds tab completions computed from the command source. */
  def suggest(f: CommandSourceStack => Iterable[String]): Arg[A] = Arg(name, brigadier, read, Some(f))

  /** Adds fixed tab completions. */
  def suggest(values: String*): Arg[A] = suggest(_ => values)

  /** Reads this argument's parsed value from the ambient [[Ctx]]. */
  inline def value(using ctx: Ctx): A = ctx(this)

object Arg:
  /** A single word without spaces. */
  def word(name: String): Arg[String] =
    Arg(name, StringArgumentType.word(), StringArgumentType.getString, None)

  /** The rest of the input, spaces included. Must be the last argument. */
  def text(name: String): Arg[String] =
    Arg(name, StringArgumentType.greedyString(), StringArgumentType.getString, None)

  def int(name: String, min: Int = Int.MinValue, max: Int = Int.MaxValue): Arg[Int] =
    Arg(name, IntegerArgumentType.integer(min, max), (c, n) => IntegerArgumentType.getInteger(c, n), None)

  /** One online player, by name or selector. */
  def player(name: String): Arg[Player] =
    Arg(
      name,
      ArgumentTypes.player(),
      (c, n) =>
        val players = c.getArgument(n, classOf[PlayerSelectorArgumentResolver]).resolve(c.getSource).asScala
        players.headOption.getOrElse(throw CommandFailure("No player matched."))
      ,
      None,
    )

  /** A word restricted to `values`, completed from them. */
  def oneOf(name: String, values: String*): Arg[String] =
    val allowed = values.toSet
    Arg(
      name,
      StringArgumentType.word(),
      (c, n) =>
        val v = StringArgumentType.getString(c, n)
        if allowed(v) then v else throw CommandFailure(s"Expected one of: ${values.mkString(", ")}."),
      Some(_ => values),
    )