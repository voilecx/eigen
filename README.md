# Eigen

Open-source ML combat anti-cheat for Paper/Folia servers.

## Modules

- `core` — feature extraction, masks, windowing and model runtime. No Bukkit/Paper dependency, so the same code runs on the server and in offline replay.
- `paper` — Paper/Folia adapter: packet listeners, scheduling, commands.

## Requirements

- Java 25 (Paper 26.1+)
- [PacketEvents](https://github.com/retrooper/packetevents) plugin installed on the server

## Build

```bash
sbt testFull paper/assembly
```

The plugin jar is written to `target/out/jvm/scala-3.9.0/eigen-paper/Eigen-<version>.jar`.
The Scala runtime is not bundled: Paper downloads it on first start via `libraries` in `plugin.yml`.

## License

GPL-3.0-only, see [LICENSE](LICENSE).
