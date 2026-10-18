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

package eigen.paper.cmd

import net.kyori.adventure.text.Component
import net.kyori.adventure.text.format.NamedTextColor
import org.bukkit.entity.Player

import eigen.commands.{Arg, CommandSpec, Ctx}
import eigen.commands.CommandTree.*
import eigen.core.sim.RotationSystem
import eigen.paper.alert.Alerts
import eigen.paper.collect.{CollectSession, Collector, Label}
import eigen.paper.garnet.GarnetBridge
import eigen.paper.net.Players
import eigen.paper.stream.StreamClient

/** The `/eigen` command tree. */
final class EigenCommands(
    players: Players,
    collector: Collector,
    stream: StreamClient,
    alerts: Alerts,
    garnet: GarnetBridge,
):
  private val Target = Arg.player("player")
  private val PlayerName = Arg.word("player")
  private val LabelArg = Arg.oneOf("label", Label.values.map(_.wire).toSeq*)
  private val Details = Arg.text("details")
  private val Toggle = Arg.oneOf("state", "on", "off")
  private val Note = Arg.text("note")

  val spec: CommandSpec = command("eigen", "Eigen anti-cheat", "ei"):
    requiresAny("eigen.command", "eigen.record")

    literal("status"):
      requires("eigen.command")
      run("connection and collection overview")(status)

    literal("alerts"):
      requires("eigen.alerts")
      run("toggle staff alerts"): ctx =>
        ctx.raw.getSource.getSender match
          case staff: Player => ctx.reply(if alerts.toggle(staff) then "Alerts on." else "Alerts off.")
          case _ => ctx.fail("Only players receive alerts.")

    literal("vl"):
      requires("eigen.alerts")
      arg(PlayerName):
        run("inspect violation count of a player")(vl)

    literal("debug"):
      requires("eigen.admin")
      arg(Target):
        run("live simulation state of a player")(debug)

    literal("exempt"):
      requires("eigen.admin")
      arg(Target):
        run("toggle streaming for a player"): ctx =>
          val p = tracked(ctx, ctx(Target))
          p.exempt = !p.exempt
          ctx.reply(s"${p.name} is ${if p.exempt then "exempt" else "no longer exempt"}.")

    literal("collect"):
      requires("eigen.collect")
      literal("start"):
        arg(Target):
          arg(LabelArg):
            run("record a labelled session")(ctx => start(ctx, ""))
            arg(Details):
              run("…with key=value tags and a note")(ctx => start(ctx, ctx(Details)))
      literal("stop"):
        arg(Target):
          run("finish a recording"): ctx =>
            val target = ctx(Target)
            collector.stop(target.getUniqueId).fold(ctx.fail, s => ctx.reply(s"Stopped ${describe(s)}."))
      literal("mark"):
        arg(Target):
          arg(Toggle):
            run("mark a cheat toggle inside a recording")(ctx => mark(ctx, ""))
            arg(Note):
              run("…with a note")(ctx => mark(ctx, ctx(Note)))
      literal("list"):
        run("recordings in progress"): ctx =>
          val active = collector.active.toSeq
          if active.isEmpty then ctx.reply("No recordings in progress.")
          else active.foreach(s => ctx.reply(describe(s)))

    literal("record"):
      requires("eigen.record")
      literal("start"):
        run("record your own gameplay as legit")(ctx => recordStart(ctx, ""))
        arg(Note):
          run("…with an optional note")(ctx => recordStart(ctx, ctx(Note)))
      literal("stop"):
        run("stop your own legit recording")(recordStop)
      literal("status"):
        run("check your calibration and recording progress")(recordStatus)

    literal("help"):
      run(ctx => help(ctx))

  private def help(ctx: Ctx): Unit = spec.usage.foreach(line => ctx.reply(line))

  private def status(ctx: Ctx): Unit =
    val tracked = players.all.size
    ctx.reply(header("Eigen"))
    ctx.reply(s"Backend: ${stream.state.toString.toLowerCase}${if stream.inference then ", inference on" else ""}")
    if garnet.enabled then
      ctx.reply(s"Garnet: ${if garnet.connected then "connected" else "connecting/offline"}")
    ctx.reply(s"Players tracked: $tracked, recordings: ${collector.active.size}")
    if stream.enabled then ctx.reply(s"Queued windows: ${stream.backlog}, dropped: ${stream.dropped}")

  private def vl(ctx: Ctx): Unit =
    val name = ctx(PlayerName)
    garnet.queryViolation(name):
      case Some(st) =>
        ctx.reply(
          f"${st.player}: VL x${st.vl} — last ${st.label} ${st.score * 100}%.1f%% (${st.model}) on ${st.server}"
        )
      case None =>
        ctx.reply(s"No violations recorded for $name.")

  private def recordStart(ctx: Ctx, note: String): Unit =
    val self = senderPlayer(ctx)
    collector
      .start(self.getUniqueId, self.getName, Label.Legit, Map("source" -> "self"), note.trim, selfService = true)
      .fold(ctx.fail, s => ctx.reply(s"Started self-recording (${describe(s)})."))

  private def recordStop(ctx: Ctx): Unit =
    val self = senderPlayer(ctx)
    collector.stopSelf(self.getUniqueId).fold(ctx.fail, s => ctx.reply(s"Stopped self-recording (${describe(s)})."))

  private def recordStatus(ctx: Ctx): Unit =
    val self = senderPlayer(ctx)
    val p = tracked(ctx, self)
    p.post: () =>
      val calibrated = RotationSystem.calibrated(p.sim.rotation)
      val gate = if p.sim.combat.gate then "combat" else "calm"
      val rec = Option(p.session).fold("not recording")(s => s"recording (${s.windows.get} windows)")
      ctx.reply(s"Calibration: ${if calibrated then "ready" else "warming up"}, gate: $gate, status: $rec")

  private def senderPlayer(ctx: Ctx): Player =
    ctx.raw.getSource.getSender match
      case player: Player => player
      case _ => ctx.fail("Only players can use /eigen record.")

  private def debug(ctx: Ctx): Unit =
    val p = tracked(ctx, ctx(Target))
    // Read on the player's network thread so the snapshot is consistent.
    p.post: () =>
      val s = p.sim
      val r = s.rotation
      val c = s.combat
      ctx.reply(header(p.name))
      val lines = Seq(
        s"client protocol ${p.profile.protocol}, tick-end ${p.profile.tickEnd}, brand ${p.brand}",
        f"rtt ${p.clock.rtt}%.0f ms, tick ${s.tick}, epoch ${s.epoch}, engagement ${c.engagement}",
        f"mouse step yaw ${r.yawAxis.currentStep}%.5f (${if r.yawAxis.converged then "ok" else "calibrating"}), pitch ${r.pitchAxis.currentStep}%.5f",
        s"gate ${if c.gate then "combat" else "calm"}, entities ${s.entities.size}",
        f"aim ${c.aimYaw}%.2f/${c.aimPitch}%.2f, distance ${c.distance}%.2f, inflation ${c.inflation}%.2f",
        s"recording: ${Option(p.session).map(describe).getOrElse("no")}, exempt ${p.exempt}",
      )
      lines.foreach(l => ctx.reply(l))

  private def start(ctx: Ctx, details: String): Unit =
    val target = ctx(Target)
    val label = Label.parse(ctx(LabelArg)).getOrElse(ctx.fail("Unknown label."))
    val (tagTokens, words) = details.split("\\s+").filter(_.nonEmpty).partition(_.contains('='))
    val tags = tagTokens.map(_.split("=", 2)).collect { case Array(k, v) if k.nonEmpty => k -> v }.toMap
    collector.start(target, label, tags, words.mkString(" ")).fold(ctx.fail, s => ctx.reply(s"Recording ${describe(s)}."))

  private def mark(ctx: Ctx, note: String): Unit =
    val target = ctx(Target)
    val on = ctx(Toggle) == "on"
    collector.mark(target.getUniqueId, on, note).fold(ctx.fail, _ => ctx.reply(s"Marked ${target.getName}: ${if on then "on" else "off"}."))

  private def tracked(ctx: Ctx, player: Player) =
    val p = players.get(player.getUniqueId)
    if p == null then ctx.fail(s"${player.getName} is not tracked.") else p

  private def describe(s: CollectSession): String =
    val tags = if s.tags.isEmpty then "" else s.tags.map((k, v) => s"$k=$v").mkString(" [", ", ", "]")
    s"${s.playerName} as ${s.label.wire}$tags, ${s.windows.get} windows"

  private def header(title: String): Component =
    Component.text(s"— $title —", NamedTextColor.DARK_AQUA)