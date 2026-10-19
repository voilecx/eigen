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
      run("toggle staff alerts"):
        sender match
          case staff: Player => reply(if alerts.toggle(staff) then "Alerts on." else "Alerts off.")
          case _ => fail("Only players receive alerts.")

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
        run("toggle streaming for a player"):
          val p = tracked(Target.value)
          p.exempt = !p.exempt
          reply(s"${p.name} is ${if p.exempt then "exempt" else "no longer exempt"}.")

    literal("collect"):
      requires("eigen.collect")
      literal("start"):
        arg(Target):
          arg(LabelArg):
            run("record a labelled session")(start(""))
            arg(Details):
              run("…with key=value tags and a note")(start(Details.value))
      literal("stop"):
        arg(Target):
          run("finish a recording"):
            collector.stop(Target.value.getUniqueId).replyOrFail(s => s"Stopped ${describe(s)}.")
      literal("mark"):
        arg(Target):
          arg(Toggle):
            run("mark a cheat toggle inside a recording")(mark(""))
            arg(Note):
              run("…with a note")(mark(Note.value))
      literal("list"):
        run("recordings in progress"):
          val active = collector.active.toSeq
          if active.isEmpty then reply("No recordings in progress.")
          else active.foreach(s => reply(describe(s)))

    literal("record"):
      requires("eigen.record")
      literal("start"):
        run("record your own gameplay as legit")(recordStart(""))
        arg(Note):
          run("…with an optional note")(recordStart(Note.value))
      literal("stop"):
        run("stop your own legit recording")(recordStop)
      literal("status"):
        run("check your calibration and recording progress")(recordStatus)

    literal("help"):
      run("show command usage")(help)

  extension [A](res: Either[String, A])
    private def replyOrFail(f: A => String)(using Ctx): Unit =
      res.fold(fail, a => reply(f(a)))

  private def help(using Ctx): Unit = spec.usage.foreach(reply)

  private def status(using Ctx): Unit =
    val tracked = players.all.size
    reply(header("Eigen"))
    reply(s"Backend: ${stream.state.toString.toLowerCase}${if stream.inference then ", inference on" else ""}")
    if garnet.enabled then
      reply(s"Garnet: ${if garnet.connected then "connected" else "connecting/offline"}")
    reply(s"Players tracked: $tracked, recordings: ${collector.active.size}")
    if stream.enabled then reply(s"Queued windows: ${stream.backlog}, dropped: ${stream.dropped}")

  private def vl(using Ctx): Unit =
    val name = PlayerName.value
    garnet.queryViolation(name):
      case Some(st) =>
        reply(
          f"${st.player}: VL x${st.vl} — last ${st.label} ${st.score * 100}%.1f%% (${st.model}) on ${st.server}"
        )
      case None =>
        reply(s"No violations recorded for $name.")

  private def recordStart(note: String)(using Ctx): Unit =
    val self = senderPlayer
    collector
      .start(self.getUniqueId, self.getName, Label.Legit, Map("source" -> "self"), note.trim, selfService = true)
      .replyOrFail(s => s"Started self-recording (${describe(s)}).")

  private def recordStop(using Ctx): Unit =
    val self = senderPlayer
    collector.stopSelf(self.getUniqueId).replyOrFail(s => s"Stopped self-recording (${describe(s)}).")

  private def recordStatus(using Ctx): Unit =
    val self = senderPlayer
    val p = tracked(self)
    p.post: () =>
      val calibrated = RotationSystem.calibrated(p.sim.rotation)
      val gate = if p.sim.combat.gate then "combat" else "calm"
      val rec = Option(p.session).fold("not recording")(s => s"recording (${s.windows.get} windows)")
      reply(s"Calibration: ${if calibrated then "ready" else "warming up"}, gate: $gate, status: $rec")

  private def senderPlayer(using Ctx): Player =
    sender match
      case player: Player => player
      case _ => fail("Only players can use /eigen record.")

  private def debug(using Ctx): Unit =
    val p = tracked(Target.value)
    // Read on the player's network thread so the snapshot is consistent.
    p.post: () =>
      val s = p.sim
      val r = s.rotation
      val c = s.combat
      reply(header(p.name))
      val lines = Seq(
        s"client protocol ${p.profile.protocol}, tick-end ${p.profile.tickEnd}, brand ${p.brand}",
        f"rtt ${p.clock.rtt}%.0f ms, tick ${s.tick}, epoch ${s.epoch}, engagement ${c.engagement}",
        f"mouse step yaw ${r.yawAxis.currentStep}%.5f (${if r.yawAxis.converged then "ok" else "calibrating"}), pitch ${r.pitchAxis.currentStep}%.5f",
        s"gate ${if c.gate then "combat" else "calm"}, entities ${s.entities.size}",
        f"aim ${c.aimYaw}%.2f/${c.aimPitch}%.2f, distance ${c.distance}%.2f, inflation ${c.inflation}%.2f",
        s"recording: ${Option(p.session).map(describe).getOrElse("no")}, exempt ${p.exempt}",
      )
      lines.foreach(reply)

  private def start(details: String)(using Ctx): Unit =
    val target = Target.value
    val label = Label.parse(LabelArg.value).getOrElse(fail("Unknown label."))
    val (tagTokens, words) = details.split("\\s+").filter(_.nonEmpty).partition(_.contains('='))
    val tags = tagTokens.map(_.split("=", 2)).collect { case Array(k, v) if k.nonEmpty => k -> v }.toMap
    collector.start(target, label, tags, words.mkString(" ")).replyOrFail(s => s"Recording ${describe(s)}.")

  private def mark(note: String)(using Ctx): Unit =
    val target = Target.value
    val on = Toggle.value == "on"
    collector.mark(target.getUniqueId, on, note).replyOrFail(_ => s"Marked ${target.getName}: ${if on then "on" else "off"}.")

  private def tracked(player: Player)(using Ctx) =
    val p = players.get(player.getUniqueId)
    if p == null then fail(s"${player.getName} is not tracked.") else p

  private def describe(s: CollectSession): String =
    val tags = if s.tags.isEmpty then "" else s.tags.map((k, v) => s"$k=$v").mkString(" [", ", ", "]")
    s"${s.playerName} as ${s.label.wire}$tags, ${s.windows.get} windows"

  private def header(title: String): Component =
    Component.text(s"— $title —", NamedTextColor.DARK_AQUA)