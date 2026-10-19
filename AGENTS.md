# AGENTS.md

Guidance for AI coding agents and human contributors working in this repository.

## Project

Eigen is an open-source, machine-learning combat anti-cheat for Minecraft servers running
Paper or Folia. This repository contains only the server plugin (the "client" of the Eigen
backend). It observes each player through network packets, models what that player's game
client sees, turns every client tick into a row of features, cuts those rows into windows and
streams them to the backend for dataset collection and inference. Training code, models and
the backend live elsewhere and are not part of this repository.

The wire contract with the backend is `docs/protocol.md`. It is the source of truth: if code
and document disagree, the code is wrong, or the document must be changed deliberately and the
protocol version bumped.

## Stack

- Scala 3 (LTS line), compiled for Java 25. sbt 2 builds the project.
- PacketEvents is a runtime plugin dependency (`depend` in `plugin.yml`); it is never shaded.
- Third-party runtime libraries (`scala3-library`, `jsoniter-scala-core`, `lettuce-core`) are
  bundled into the fat jar by `sbt paper/assembly` so the distributed jar is self-contained.
- Compile-time-only libraries (macros) are `Provided`: MacWire for wiring the object graph,
  jsoniter-scala macros for JSON codecs.

## Commands

```bash
sbt testFull            # all tests (plain `sbt test` is incremental in sbt 2)
sbt paper/assembly      # the plugin jar
```

Golden protocol fixtures live in `docs/fixtures` and are shared with the backend's tests. After
an intentional wire change, regenerate them (the sbt server must be restarted to see the
variable):

```bash
sbt shutdown && EIGEN_WRITE_FIXTURES=1 sbt core/testFull
```

`paper/src/test/.../StreamLiveSuite` runs against a live backend when `EIGEN_TEST_BACKEND` and
`EIGEN_TEST_TOKEN` are set, and is skipped otherwise.

## Layout

- `core` — platform-independent model: feature schema, per-player simulation, lag
  compensation, tick history, windows and the wire codec. It must never depend on Bukkit,
  Paper or PacketEvents, so it can be tested in isolation and reused offline.
- `config` — a small declarative DSL for typed configuration reads: each key has a type, a
  default and a check, and a section is a `for` comprehension over its keys. No platform code.
- `commands` — a small Scala DSL over Paper's Brigadier command API.
- `paper` — the adapter: packet listeners, transport, dataset collection, commands, wiring.
- `docs` — the protocol specification and its byte-level fixtures.

## Architecture

- **Components and systems.** Per-player state is split into small mutable components with
  primitive fields. Packet handlers only record facts into components. Once per client tick a
  fixed schedule of stateless systems derives features and writes one row. Keep new logic in
  this shape: a component for state, a system for derivation, a clear place in the schedule.
- **Column-major history.** Rows are stored in a ring buffer of per-column arrays. Windows are
  copied out with bulk array copies, matching the column-major wire format.
- **Thread confinement.** A player's simulation is touched only by that connection's network
  thread. Other threads hand work over through the player's mailbox or through transaction
  tasks; never mutate simulation state from another thread.
- **Lag compensation.** Outbound packets that change what the client sees are bracketed with
  transactions; their effect is applied when the client acknowledges them. Our transaction ids
  are positive; GrimAC uses non-positive ids, so both can run on one server. Never consume or
  cancel a pong that is not ours.
- **Client versions.** Old clients connect through ViaVersion. Version-specific behaviour is
  captured once in `ClientProfile`; simulation code reads the profile, never the version.

## Engineering rules

- Correctness first, then clarity, then performance. The per-tick path must not allocate:
  reuse scratch objects, prefer primitive arrays and `while` loops, use `inline` and opaque
  types for zero-cost abstractions. Ordinary Scala is fine off the hot path.
- Features must be invariant to things that identify a server, map, kit or connection
  (absolute positions, ping, client version, brand). Such values may only appear as metadata
  columns, which are never model inputs.
- The schema, mask reasons, item kinds and window kinds are part of the dataset format.
  Append only; never renumber or reorder existing entries without a schema version bump.
- Validate everything read from the network; never let malformed input throw on the network
  thread.
- No player names or raw UUIDs leave the server; players are identified by a salted HMAC.
- Every source file starts with the GPL-3.0 header. Files derived from GrimAC or Shard keep the
  attribution block naming those projects and their authors; keep it when editing.
- Tests accompany behaviour: pure logic in `core` gets unit tests, wire changes get fixture
  updates.
- Comments explain intent and invariants, not what the next line does.

## Working style for agents

- Read `docs/protocol.md` and the code you are about to change first.
- Make the smallest change that fully solves the task; do not refactor unrelated code.
- Keep the build warning-free and run `sbt testFull` before finishing.
- If a requirement is ambiguous or conflicts with these rules, stop and ask.