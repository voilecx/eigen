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

import java.util.logging.Level

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

import com.mojang.brigadier.Command
import com.mojang.brigadier.arguments.ArgumentType
import com.mojang.brigadier.builder.ArgumentBuilder
import com.mojang.brigadier.context.CommandContext
import com.mojang.brigadier.exceptions.CommandSyntaxException
import com.mojang.brigadier.tree.LiteralCommandNode
import io.papermc.paper.command.brigadier.{CommandSourceStack, Commands}
import io.papermc.paper.plugin.lifecycle.event.types.LifecycleEvents
import net.kyori.adventure.text.Component
import net.kyori.adventure.text.format.NamedTextColor
import org.bukkit.command.CommandSender
import org.bukkit.plugin.java.JavaPlugin

/** Thrown from an executor to stop with a message to the sender. */
final class CommandFailure(message: String) extends RuntimeException(message, null, false, false)

/** What an executor sees: parsed arguments and the sender. */
final class Ctx private[commands] (val raw: CommandContext[CommandSourceStack]):
  def apply[A](arg: Arg[A]): A = arg.read(raw, arg.name)
  def sender: CommandSender = raw.getSource.getSender
  def reply(message: Component): Unit = sender.sendMessage(message)
  def reply(message: String): Unit = reply(Component.text(message, NamedTextColor.GRAY))
  def fail(message: String): Nothing = throw CommandFailure(message)

/** A node of the command tree under construction. */
final class Node private[commands] (private[commands] val label: Either[String, Arg[?]]):
  private[commands] val children = ArrayBuffer[Node]()
  private[commands] var permissions: Seq[String] = Nil
  private[commands] var executor: Option[Ctx => Unit] = None
  private[commands] var help: String = ""

/** A complete top-level command, ready to register. */
final class CommandSpec private[commands] (val name: String, val description: String, val aliases: Seq[String], root: Node):

  /** Builds the Brigadier tree. `onError` reports unexpected executor failures. */
  def build(onError: (String, Throwable) => Unit): LiteralCommandNode[CommandSourceStack] =
    convert(root, onError).asInstanceOf[com.mojang.brigadier.builder.LiteralArgumentBuilder[CommandSourceStack]].build()

  /** One line per runnable path, for help output. */
  def usage: Seq[String] =
    val out = ArrayBuffer[String]()
    def walk(n: Node, prefix: String): Unit =
      val here = n.label match
        case Left(literal) => s"$prefix $literal".trim
        case Right(arg) => s"$prefix <${arg.name}>"
      if n.executor.isDefined then out += (if n.help.isEmpty then here else s"$here — ${n.help}")
      n.children.foreach(walk(_, here))
    walk(root, "/")
    out.toSeq.map(_.replaceFirst("^/ ", "/"))

  private def convert(n: Node, onError: (String, Throwable) => Unit): ArgumentBuilder[CommandSourceStack, ?] =
    val builder: ArgumentBuilder[CommandSourceStack, ?] = n.label match
      case Left(literal) => Commands.literal(literal)
      case Right(arg) =>
        val b = Commands.argument(arg.name, arg.brigadier.asInstanceOf[ArgumentType[Any]])
        arg.completions.foreach: complete =>
          b.suggests: (ctx, sb) =>
            val typed = sb.getRemainingLowerCase
            complete(ctx.getSource).iterator.filter(_.toLowerCase.startsWith(typed)).foreach(sb.suggest)
            sb.buildFuture()
        b
    if n.permissions.nonEmpty then
      val perms = n.permissions
      builder.requires(src => perms.exists(src.getSender.hasPermission))
    n.executor.foreach: run =>
      builder.executes: (ctx: CommandContext[CommandSourceStack]) =>
        val c = Ctx(ctx)
        try
          run(c)
          Command.SINGLE_SUCCESS
        catch
          case f: CommandFailure =>
            c.reply(Component.text(f.getMessage, NamedTextColor.RED))
            0
          // Parse and resolve errors (unknown player, bad selector): Brigadier reports them.
          case e: CommandSyntaxException => throw e
          case e: Exception =>
            onError(s"/$name", e)
            c.reply(Component.text("Internal error, see the server log.", NamedTextColor.RED))
            0
    n.children.foreach(child => builder.`then`(convert(child, onError)))
    builder

/**
 * A small DSL for Paper's Brigadier commands. Blocks nest with context functions (`Node ?=> Unit`
 * for tree structure and `Ctx ?=> Unit` for execution), so the code mirrors the command tree:
 *
 * {{{
 * val Target = Arg.player("player")
 * command("eigen", "Eigen anti-cheat"):
 *   requires("eigen.command")
 *   literal("status"):
 *     run(reply("ok"))
 *   literal("debug"):
 *     arg(Target):
 *       run(reply(Target.value.getName))
 * }}}
 */
object CommandTree:

  def command(name: String, description: String, aliases: String*)(body: Node ?=> Unit): CommandSpec =
    val root = Node(Left(name))
    body(using root)
    CommandSpec(name, description, aliases, root)

  def literal(name: String)(body: Node ?=> Unit)(using parent: Node): Unit =
    val n = Node(Left(name))
    body(using n)
    parent.children += n

  def arg[A](a: Arg[A])(body: Node ?=> Unit)(using parent: Node): Unit =
    val n = Node(Right(a))
    body(using n)
    parent.children += n

  /** Restricts this node and everything below it to senders with `permission`. */
  def requires(permission: String)(using n: Node): Unit = n.permissions = Seq(permission)

  /** Restricts this node and everything below it to senders with any of `permissions`. */
  def requiresAny(permissions: String*)(using n: Node): Unit = n.permissions = permissions

  /** Makes this node runnable with `Ctx` provided as a context parameter. `help` shows up in [[CommandSpec.usage]]. */
  def run(body: Ctx ?=> Unit)(using n: Node): Unit =
    n.executor = Some(c => body(using c))

  def run(help: String)(body: Ctx ?=> Unit)(using n: Node): Unit =
    n.help = help
    n.executor = Some(c => body(using c))

  /** Accesses the ambient command context inside a [[run]] block. */
  inline def ctx(using c: Ctx): Ctx = c

  /** Accesses the command sender from the ambient [[Ctx]]. */
  inline def sender(using c: Ctx): CommandSender = c.sender

  /** Replies to the ambient command sender. */
  inline def reply(message: Component)(using c: Ctx): Unit = c.reply(message)
  inline def reply(message: String)(using c: Ctx): Unit = c.reply(message)

  /** Aborts the command with an error message sent to the ambient sender. */
  inline def fail(message: String)(using c: Ctx): Nothing = c.fail(message)

  /** Registers commands with Paper's lifecycle so they survive `/reload` of the datapacks. */
  def register(plugin: JavaPlugin, specs: CommandSpec*): Unit =
    val onError = (where: String, e: Throwable) => plugin.getLogger.log(Level.SEVERE, s"Command $where failed", e)
    plugin.getLifecycleManager.registerEventHandler(
      LifecycleEvents.COMMANDS,
      event => specs.foreach(s => event.registrar().register(s.build(onError), s.description, s.aliases.asJava)),
    )