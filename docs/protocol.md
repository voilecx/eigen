# Eigen stream protocol, version 1

This document is the contract between the Eigen server plugin (client) and the
Eigen backend (server). Both sides must implement exactly what is written here;
when the code and this document disagree, the document wins and the code is a bug.

## 1. Transport

- One WebSocket connection per Minecraft server process, over TLS (`wss://`).
  Plain `ws://` is accepted only for loopback development.
- Endpoint: `GET /v1/stream`.
- Authentication happens during the HTTP upgrade, before the socket is accepted:
  `Authorization: Bearer <token>`. A missing, malformed, unknown or revoked token
  is answered with HTTP `401` and the upgrade is refused.
- The request also carries `Eigen-Protocol: 1`. Any other value is answered with
  HTTP `426`.
- Every protocol message is one WebSocket **binary** message. Text messages are a
  protocol error. WebSocket ping/pong is used for liveness: the server pings every
  15 s and closes the connection after 45 s without a pong.

### 1.1 Tokens

```
eig_live_<30 base62 chars><6 base62 chars>
```

- The 30-character body is drawn from a CSPRNG (about 178 bits).
- The last 6 characters are CRC-32 (IEEE) of the body, written in base62
  (`0-9A-Za-z`), left-padded with `0`. The checksum only proves well-formedness;
  it is checked before any database lookup so that garbage never reaches storage.
- The server stores only `sha256(token)` (lowercase hex) and the last four
  characters for display. The token itself is shown once, when issued.
- Regex for secret scanners: `\beig_live_[0-9A-Za-z]{36}\b`.

## 2. Frame

All integers are little-endian. Every message starts with an 8-byte header:

| Offset | Size | Field | Meaning |
|---|---|---|---|
| 0 | 1 | `type` | Message type, see §3 |
| 1 | 1 | `flags` | bit 0: payload is raw DEFLATE (RFC 1951) compressed; other bits must be 0 |
| 2 | 2 | `reserved` | Must be 0 |
| 4 | 4 | `seq` | Client sequence number (u32), see §2.1; 0 where not applicable |
| 8 | … | `payload` | Type-specific |

A frame larger than `max_frame_bytes` (announced in `WELCOME`, after decompression)
is a fatal protocol error.

### 2.1 Sequencing and acknowledgement

- `stream_id` is a random 64-bit value the client generates once per process start
  and sends in `HELLO`. It survives reconnects of the same process.
- Every client→server message except `HELLO` carries a `seq`. The first one is 1;
  each next one is exactly the previous plus 1, across reconnects of the stream.
- The server applies messages strictly in `seq` order. A message whose `seq` is
  not greater than the last applied one is a duplicate: it is acknowledged and
  ignored. A gap (`seq` greater than last applied + 1) is a fatal protocol error.
- `ACK` carries the highest `seq` that is durably applied (cumulative). The server
  persists this value per `stream_id`, so it survives server restarts.
- The client keeps every unacknowledged message in a bounded resend buffer. After a
  reconnect, `WELCOME.resume_from` tells it the last applied `seq`; it drops
  everything up to that and resends the rest in order before sending new messages.
- If the resend buffer overflows, the client drops the oldest **window** messages
  first and never drops control messages; it then starts a new `stream_id`
  (the server treats it as a new stream).

## 3. Message types

| Type | Name | Direction | Payload |
|---|---|---|---|
| `0x01` | `HELLO` | C→S | JSON |
| `0x02` | `PLAYER_JOIN` | C→S | JSON |
| `0x03` | `PLAYER_LEAVE` | C→S | JSON |
| `0x04` | `COLLECT_START` | C→S | JSON |
| `0x05` | `COLLECT_STOP` | C→S | JSON |
| `0x06` | `MARK` | C→S | JSON |
| `0x10` | `WINDOWS` | C→S | binary, §5 |
| `0x81` | `WELCOME` | S→C | JSON |
| `0x82` | `ACK` | S→C | binary: `u32 seq` |
| `0x83` | `VERDICT` | S→C | JSON |
| `0x8F` | `ERROR` | S→C | JSON |

JSON payloads are UTF-8 objects. Unknown fields are ignored by the receiver.

### 3.1 `HELLO` (first message after the upgrade, `seq = 0`)

```json
{
  "protocol": 1,
  "stream_id": "9f86d081884c7d65",
  "plugin": "eigen 0.1.0",
  "platform": "Paper 26.2",
  "schema": { "...": "manifest, §4" },
  "schema_hash": "<crc32 of the schema bytes>"
}
```

`schema_hash` is the CRC-32 of the UTF-8 bytes of the canonical manifest JSON (§4),
lowercase hex, 8 digits.

### 3.2 `WELCOME`

```json
{
  "protocol": 1,
  "resume_from": 0,
  "max_frame_bytes": 4194304,
  "server_time_ms": 1792000000000,
  "inference": false
}
```

`inference: true` means the server runs a model and the client should mark windows
with the `infer` purpose (§5). The client sends nothing but `HELLO` until it has
received `WELCOME`.

### 3.3 Player and dataset messages

```json
// PLAYER_JOIN
{ "handle": 7, "player": "3b1f…(64 hex)", "client_protocol": 771, "brand": "fabric" }
// PLAYER_LEAVE
{ "handle": 7 }
// COLLECT_START
{ "session": 3, "session_uuid": "6f0c…", "handle": 7, "label": "cheat",
  "tags": { "client": "x", "module": "killaura" }, "note": "", "started_at_ms": 0 }
// COLLECT_STOP
{ "session": 3, "stopped_at_ms": 0 }
// MARK
{ "session": 3, "tick": 18234, "state": "on", "note": "" }
```

- `handle` is a u32 the client assigns to a player for the lifetime of the stream.
- `player` is a pseudonymous player id: `hmac_sha256(server_salt, uuid_bytes)` in
  lowercase hex. The raw UUID and the player name never leave the Minecraft server.
- `session` is a u32 the client assigns to a dataset session for the lifetime of the
  stream; `session_uuid` is its globally unique id.
- `label` is `legit`, `cheat` or `unknown`. `tags` is free-form string→string.
- `MARK` records a segment boundary inside a session (for example, the tester turned
  a cheat module on). `tick` is the client tick index (§5.2) of the boundary.

### 3.4 `VERDICT`

```json
{ "handle": 7, "window": 912, "model": "aim-v3", "scores": { "aimbot": 0.93, "legit": 0.07 } }
```

Scores are calibrated probabilities. The client decides what to do with them.

### 3.5 `ERROR`

```json
{ "code": "PROTOCOL", "message": "seq gap: expected 41, got 43", "fatal": true }
```

Codes: `UNAUTHORIZED`, `PROTOCOL`, `SCHEMA`, `OVERLOADED`, `INTERNAL`.
After a fatal error the server closes the connection.

## 4. Schema manifest

The manifest describes the column layouts used in `WINDOWS`. It is generated from
the plugin's schema code and is the single source of truth for column order.

```json
{
  "version": 1,
  "layouts": [
    { "id": 1, "name": "combat", "columns": [ ["dots_yaw", "f32", "rot"], ["strafe", "i8", "inp"] ] },
    { "id": 2, "name": "calm", "columns": [ ["dots_yaw", "f32", "rot"] ] }
  ]
}
```

- Each column is `[name, type, stream]`.
- Types and their sizes: `u8` 1, `i8` 1, `u16` 2, `i16` 2, `u32` 4, `f32` 4.
- Streams: `rot` (mouse), `inp` (keys and clicks), `tgt` (target geometry),
  `mask` (per-stream validity, 1 = valid), `meta` (context that must not be used as
  model input: gate state, mask reasons, uncertainty, item and target kinds).
- Canonical JSON: keys in the order shown, no whitespace.

## 5. `WINDOWS` payload

```
u16  count
repeat count times:
  u32  handle        player handle (§3.3)
  u32  window        window id, unique and increasing within the stream
  u32  session       dataset session (§3.3), 0 if the window is not collected
  u8   kind          1 = attack, 2 = engage, 3 = calm
  u8   purpose       bit 0 = collect (persist to the dataset), bit 1 = infer
  u8   layout        layout id from the manifest
  u8   reserved      0
  u32  epoch         continuity id of the player's tick timeline, §5.3
  u32  engagement    engagement id, §5.3
  u32  first_tick    client tick index of row 0
  u16  rows          number of rows (1..4096)
  i16  anchor        row index of the anchor event, -1 for calm windows
  columns            for each column of the layout, in manifest order:
                     rows values of that column, little-endian, packed
```

The size of a window is fully determined by its header and the manifest. A payload
whose length does not match is a fatal protocol error.

### 5.1 Window kinds

- `attack`: fixed-length window around a melee hit on a target (`anchor` = hit row).
- `engage`: fixed-length window around the first tick of an engagement, the moment
  a target first becomes hittable after a calm period (`anchor` = that row).
- `calm`: a contiguous run of non-combat ticks, variable length. Calm windows use
  the low-dimensional `calm` layout.

### 5.2 Ticks

`first_tick` and `MARK.tick` count client ticks, as delimited by the client's
`CLIENT_TICK_END` packet, starting from 0 when the player joins. Rows of a window are
consecutive client ticks. A world change or respawn starts a new epoch (§5.3); windows
never span it.

### 5.3 Linking combat and calm data

Training pairs a player's combat windows with that same player's calm behaviour
(the calm data is the per-player baseline). The client therefore labels every window
with enough context to rebuild the timeline exactly:

- `epoch` (u32, starts at 1 per handle) increases whenever the player's tick timeline
  is broken: join, respawn, world change. Windows of the same handle and epoch lie on
  one continuous tick axis and can be ordered by `first_tick`.
- `engagement` (u32, starts at 1 per handle, 0 = none yet) increases each time the
  player enters combat after a calm period, that is, at every `engage` anchor.
  - `attack` and `engage` windows carry the engagement they belong to.
  - `calm` windows carry the most recent engagement that ended before the segment
    started (0 if the player has not fought yet). The counter is per handle and keeps
    increasing across epochs.

So for engagement `E` of a player, the calm segments before it are the calm windows
with the same handle and epoch and `engagement < E`, and the ones right after it carry
`engagement = E`. Across sessions and reconnects, the stable pseudonymous `player` id
(§3.3) links a player's data.
