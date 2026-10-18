# Eigen

Open-source machine-learning combat anti-cheat for Paper and Folia servers.

Eigen models each player's client from the packets it exchanges with the server: mouse
movement in sensitivity-independent counts, movement keys, clicks, and the geometry of the
player's aim at their target, with lag compensation for what the client actually sees. Every
client tick becomes a row of features; rows are cut into combat windows (around hits and the
start of fights) and calm segments (the player's own baseline), and streamed to the Eigen
backend for dataset collection and inference.

## Requirements

- Paper or Folia 26.1+ on Java 25
- [PacketEvents](https://github.com/retrooper/packetevents) installed as a plugin
- Clients of any version: 1.21.2+ clients are observed tick by tick; older clients connecting
  through ViaVersion are supported with version-specific handling

## Commands

| Command | Permission | |
|---|---|---|
| `/eigen status` | `eigen.command` | Backend connection and collection overview |
| `/eigen alerts` | `eigen.alerts` | Toggle staff alerts for backend verdicts |
| `/eigen vl <player>` | `eigen.alerts` | Inspect a player's violation count and latest verdict |
| `/eigen debug <player>` | `eigen.admin` | Live simulation state of a player |
| `/eigen exempt <player>` | `eigen.admin` | Stop streaming a player |
| `/eigen collect start <player> <legit\|cheat\|unknown> [key=value…] [note]` | `eigen.collect` | Record a labelled session |
| `/eigen collect mark <player> <on\|off> [note]` | `eigen.collect` | Mark a cheat toggle inside a recording |
| `/eigen collect stop <player>` | `eigen.collect` | Finish a recording |
| `/eigen collect list` | `eigen.collect` | Recordings in progress |
| `/eigen record start [note]` | `eigen.record` | Self-record your own gameplay as `legit` |
| `/eigen record stop` | `eigen.record` | Stop your own self-service recording |
| `/eigen record status` | `eigen.record` | Check your mouse calibration and recording status |

Recordings go to the backend, to `plugins/Eigen/collect/` or both (`collect.sink` in
`config.yml`). The local format matches what the backend stores.

## Build

```bash
sbt testFull paper/assembly
```

The jar is written to `target/out/jvm/scala-3.9.0/eigen-paper/Eigen-<version>.jar`. It contains
only Eigen's classes; Paper downloads the Scala runtime and other libraries on first start.

## Protocol

The plugin talks to the backend over a single authenticated WebSocket stream, described in
[docs/protocol.md](docs/protocol.md).

## Credits

Eigen stands on the work of two open-source anti-cheats, and parts of it are derived from
their code (each such file says so in its header):

- [GrimAC](https://github.com/GrimAnticheat/Grim) by DefineOutside and contributors — the
  transaction-based lag compensation and entity interpolation model.
- [Shard](https://github.com/KaelusAI/Shard) by KaelusAI — the mouse sensitivity estimation,
  aim geometry and the overall approach of ML detection on per-tick packet data.

## License

GPL-3.0-only, see [LICENSE](LICENSE).