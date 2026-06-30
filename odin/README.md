# Odin in Habu

Port of Odin's pure-data / analysis layer from Zig to checked Habu, built on the
Habu stdlib FFI/float/fmt infrastructure (on `master`). The goal is a typed,
checker-verified DSL for the parse → metrics → render pipeline, with the
SDK-bound capture/detector core reachable later through `lib/ffi.f`.

## Package architecture

`odin` is the **public interface**; each analyzer is an internal Habu **package**
(a real wordlist namespace). External callers use `ODIN:WORD`; a module's own files
(source + renderer + emit + tests) reopen `package NAME` and call its words
**unqualified**; cross-package calls qualify (`OTHER:WORD`) or reopen the other
package. The package rules live in `docs/forth.md`.

| package    | files                                  | analyzer |
|------------|----------------------------------------|----------|
| `TEGRA`    | tegrastats                             | tegrastats line parser → `TEGRA:SUMMARY` |
| `NETPBM`   | netpbm                                 | P5/P6 image I/O |
| `YOLO`     | yolo-decode                            | detection decode + greedy NMS |
| `LUMA`     | luma-hist                              | luminance histogram |
| `CONFIG`   | config                                 | rig config validation |
| `EXPOSURE` | exposure-metrics                       | exposure / luma metrics |
| `LATCAL`   | latency-calibration                    | latency calibration stats |
| `XCORR`    | latency-xcorr                          | latency cross-correlation |
| `SCHEMA`   | capture-schema (+ -json)               | NDJSON capture schema validation |
| `ODREC`    | live-records                           | capture/live detector JSON rows → checked record structures |
| `CAMSYNC`  | timestamp-metrics / -phase / -render   | camera-capture timestamp sync (TM per-camera, TG frame-group, TX cross-camera) |
| `FPS`      | fps-sweep / fps-report                 | per-camera FPS quality |
| `TRACKER`  | tracker (+ -render)                    | multi-object tracker |
| `LOWLIGHT` | low-light-report / -manifest           | low-light metrics + manifest |
| `PERCEPTION`| perception-latency / -render / -analyze | perception latency metrics |
| `CAMTRACK` | camera-tracker                         | alpha-beta camera-rate tracker |

**Naming rule:** single-analyzer packages naturalize their words (no module
prefix — `TEGRA:SUMMARY`, `NETPBM:DECODE`, `CAMTRACK:RESET`). Multi-sub-analyzer
packages (`CAMSYNC` `TM-`/`TG-`/`TX-`, plus `FPS`/`TRACKER`/`LOWLIGHT`/`PERCEPTION`)
keep their sub-prefixes inside the package to distinguish coupled sub-analyzers
that would otherwise self-collide. The `end-to-end-test` integration reopens
`package CAMSYNC` and qualifies the `SCHEMA:` parse calls.

Standalone tools stay **global** (loaded alone, run on load): `grouping-bench.f`
(O(n²)→O(1) hashmap benchmark) and the SDK probe `zed-ffi.f`.
Shared rendering comes from `lib/render.f` / `lib/report.f` on master (not vendored).

> The per-module prose below predates the package rename and still shows the old
> prefixed word names (e.g. `NP-DECODE`, `CS-*`, `YD-DECODE`); the algorithms and
> oracles are unchanged — only the namespacing moved (now `NETPBM:DECODE` etc.).

## Modules

- `tegrastats.f` — one-line `tegrastats` parser (RAM/SWAP/CPU%/GPU%/max-temp/power
  → canonical summary). Ported from Odin `src/tegrastats.zig`; verified against
  that file's exact ohsnap oracle. CPU/GPU/temp are kept as integer tenths so the
  pipeline stays integer (no float-cell storage, no trust).
- `netpbm.f` — P5/P6 (PGM/PPM) image I/O: `WRITE-P5` packs an 8-bit grayscale
  image; `NP-DECODE` parses a binary P5/P6 (with `#` comments) into its luma
  plane (P6 RGB → luma via the Zig weights). Ported from `src/netpbm.zig`,
  verified against its tests.
- `yolo_decode.f` — full detection decode + greedy NMS, ported from
  `src/yolo_decode.zig` `decode()`. Geometry (`OVERLAP1D`/`IOU`/`LETTERBOX`/
  `FCLAMP`/`DECODE-BOX`) plus `YD-DECODE` (per-anchor argmax over classes →
  confidence threshold → letterbox-unmap box decode → validity) and `YD-NMS`
  (greedy non-max suppression). Detections live in parallel float-cell arrays;
  NMS is done by selection — repeatedly take the highest-confidence unprocessed
  detection — which yields exactly the sort-then-greedy result without sorting
  floats. Verified against both "iou and overlap" and the "decode + NMS of a
  synthetic raw buffer" snapshot (2 kept boxes; the lower-confidence duplicate
  suppressed).
- `luma_hist.f` — 256-bin luminance histogram + mean/percentile luminance,
  ported from `src/low_light.zig` ImageAggregate. Pure integer; feeds off
  `netpbm.f` NP-DECODE's luma plane. Verified against hand-computed oracles.
- `config.f` — camera-rig config validation (identity, unique serials, full
  4-camera rig, localization readiness) returning `config.zig`-style error codes.
  Ported from `src/config.zig`, verified against its rig-validation tests.
- `capture_schema.f` — NDJSON record classification + exact schema-version
  checks, from `src/capture_schema.zig` plus Odin live detector stream schemas.
  `odin.capture.v1` records use the capture `type` field; live detector records
  (`odin.localization_detections.v1`, `odin.perception_tick.v1`, and
  `odin.tracker_tick.v1`) dispatch by `schema_version` because they intentionally
  have no capture `type`.
- `capture_schema-json.f` — the full `validateObject`/`validateLine` contract over
  habu `tools/json.f`: wrong-timestamp-unit rejection, required `type` +
  `schema_version` for capture records, schema-only dispatch for live
  detector/tick records, and every record type's required fields with their JSON
  kind (string/integer/number/bool/array/object + nullable variants) plus the
  `time_reference`/`result` enum checks. Returns the `capture_schema.zig`
  `ValidationError` set as `CS-*` status codes. The `.integer` vs `.number`
  distinction is a number-text shape test (no `.`/`e`/`E`), since json.f keeps
  numbers as text spans. Verified against an expanded oracle covering capture
  records, live detection/tick records, nullable detection boxes, timestamp-unit
  rejection, unsupported live schemas, and field-type/missing-field failures.
  This is the JSON access layer the NDJSON analyzers
  (timestamp/latency/fps/low-light/tracker) build on.
- `live-records.f` — checked record structures and JSON loaders for the
  validated handoff rows used by live capture/detector analysis:
  `FRAME-REC`, `DETECTION-REC`, `PERCEPTION-TICK-REC`, and
  `TRACKER-TICK-REC`. Pointer-valued string fields are exposed through typed
  `ptr-field` wrappers (`FRAME.SERIAL-A`, `DET.SOURCE-A`, etc.) so analyzer
  kernels can read rows without reverting to untyped JSON lookups or parallel
  scratch arrays. The loader tests validate each row through `SCHEMA` first,
  parse it with `tools/json.f`, fill the checked structures, and feed frame
  records into `CAMSYNC:TM-ADD` to prove structure-to-analyzer wiring.

- `timestamp_metrics.f` — per-camera frame-timing kernel from
  `src/timestamp_metrics.zig` (`CameraTiming`/`updateCameraTiming`/
  `addPeriodSample`/`finish`): folds frame records (keyed by serial in parallel
  arrays) into period sample count, sdk/host period mean/min/max + max jitter vs
  target period, dropped/duplicate/regression flag counts, frame-index gap drops,
  and summary `frames_dropped`. Periods sampled only across monotonic frames.
  It also builds the cross-camera skew (`buildCrossCameraSkew` +
  `FrameIndexGroup`): frames grouped by frame_index, complete/incomplete index
  sets vs camera count, common-lifecycle detection from schema commands, and
  SDK/host skew mean/min/max plus p95/p99 nearest-rank (over the skew array
  sorted with the generic `lib/sort.f` `SORT!`) over complete sets. Verified
  against both the "collate frame periods and gaps by camera" (common lifecycle,
  skew present) and "do not compute cross skew for sequential helper streams" (no
  common lifecycle, skew suppressed) tests, with p95/p99 checked against the
  nearest-rank formula on the oracle data. Frame-index grouping is backed by
  `lib/hashmap.f` (O(1) `HM-PROBE` lookup, capacity-guarded with `E-TG-FULL` —
  never a silent cap), so collation is O(frames) not O(frames·groups). Measured
  head-to-head (`odin/grouping-bench.f`, 16 000 distinct keys): linear scan
  5.357s vs hashmap 0.877s on this Jetson-class host. Timestamp renderers are
  byte-exact against the Zig oracle.
- `timestamp_phase.f` — frame phase-offset analysis from `src/timestamp_metrics.zig`
  (`buildFramePhaseOffsets`/`bestFramePhaseOffset`/`phaseOffsetStats`). Frame
  samples are indexed in an O(1) hash keyed by `(frame_index<<4 | camera_index)`,
  so the offset sweep's per-(camera,frame) timestamp lookup is O(1), not a linear
  scan. For each camera vs the reference it tries offsets −2..2, scoring by the
  SDK-skew distribution (mean/p95/p99/max via `SORT!`+`PCTL`), keeping the
  smallest-p99 offset (ties → more matched frames). Verified vs test 1700
  ("one-frame camera phase": offset −1, 333 ns skew, same-index p99 16667000).

- `fps_sweep.f` — per-camera FPS quality metrics from `src/fps_sweep.zig`
  (`achievedFpsMilli` + `summarizeCaseQuality` + the ratio comparators): for one
  capture case, the slowest/fastest camera milli-FPS, camera FPS spread,
  cameras-at-target count, and worst per-camera drop rate as an exact num/den
  ratio (cross-multiplied, never a rounded float) — the multi-camera FPS
  conclusion metric this project mandates. Verified against a hand-derived oracle
  from the `.zig` formulas. The mode-ranking decision (`allModeBetter`) and the
  report renderer are the next steps.

- `tracker.f` — multi-object tracker from `src/tracker.zig`: BBox IoU +
  Euclidean distance geometry, plus the full per-frame association — `FD-ADD`
  buffers a frame's detections, `TK-FRAME` builds distance-gated (+ optional IoU)
  candidate (track,det) pairs, assigns them greedily by ascending distance
  (selection == sort-then-greedy), updates matched tracks (confirming at
  `confirm_hits`), spawns tracks for unmatched detections, and ages tracks past
  `max_age`. Tracks live in parallel arrays; distances are squared (no sqrt) for
  gating/selection. Verified against "iou and distance" and "single moving target
  keeps one stable confirmed track" (records/created/confirmed/matched/new). Track
  length stats and the JSON track-update emission are the next steps.

- `low_light_manifest.f` — low-light scenario manifest validation from
  `src/low_light.zig` (`parseLowLightManifest`/`validateLowLightManifest`): exact
  schema version, eight required non-empty (trimmed) strings, a `target_proxy`
  sub-object (description + contrast_reference non-empty; optional physical
  width/height/range/angular must be positive), and `repeats != 0`. Built on the
  JSON layer (`tools/json.f`) + `lib/float.f` `STR>FLOAT` for the numeric checks.
  Verified against "parses scenario manifest" + "rejects invalid manifests"
  (valid → ok, schema v2 → unsupported, negative range → invalid). Together with
  `luma_hist.f` (the image-statistics core) this covers low_light's pure-data layer.
- `zed_ffi.f` — **SDK reachability through `ffi-call-n`**: dlopens the real ZED SDK
  (`/usr/local/zed/lib/libsl_zed.so`), resolves the extern-"C"
  `getZEDSDKRuntimeVersion_C(int*,int*,int*)`, and calls it via `lib/ffi.f`
  `FFI-CALLN`. Non-invasive (no camera). Validated on the standing `bin/hb` with
  SDK runtime version `5.2.3`.

- `perception_latency.f` — perception latency metric core from
  `src/perception_latency.zig`: collect per-record `latency_ms` samples, sort, and
  report `latency_samples` + p50/p95/p99/max (nearest-rank `percentileF64`:
  `sorted[(len*pct+99)/100-1]`, max = last). Float samples sort via `lib/sort.f`
  `FSORT!`. The `.zig` carries no inline test, so the oracle is the reference
  itself — `percentileF64` was run directly under zig 0.16 on known sample sets and
  the Habu port is checked against those outputs. (Rates, queue depth, tracker
  latency, and timing summaries are the same reductions over their own samples.)
  `perception-analyze.f` is the file-level bridge: it reads JSONL with
  `tools/json-file.f`, validates each row through `SCHEMA:VALIDATE-LINE`, loads
  checked `ODREC` live records, copies stable camera/target strings out of the
  parser buffer, and populates the renderer report. Its regression fixture matches
  the Zig `perception-latency` sample metrics CSV and camera-metrics CSV.

## Running tests

`bin/hb` is a symlink to the built engine. Each `*-test.f` carries its exact load
chain in a `\ Run:` header comment (tests reopen their own `package` and call the
module unqualified); run that chain from the workspace root, e.g.:

```sh
cat lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f \
    odin/tegrastats.f odin/tegrastats-test.f | bin/hb        # -> test: ok
```

The six byte-exact renderer comparisons were run by loading the checked Habu emit
drivers and diffing their stdout against the `.zig` reference oracles (inline
ohsnap snapshots, except `perception-render` whose `.zig` carries no inline test
and was compared against `src/pl_oracle_harness.zig` run under zig 0.16):

```text
odin/timestamp-render-emit.f
odin/fps-report-emit.f
odin/tracker-render-emit.f
odin/low-light-report-emit.f
odin/camera-tracker-emit.f
odin/perception-render-emit.f
```

(`lib/float.f` is loaded wherever `lib/fmt.f` is, because `fmt.f` depends on its
`POW10` — even for integer-only modules.)

### SDK reachability (proven)

`odin/zed-ffi.f` reaches the real ZED SDK on the **standing** `bin/hb`: load
`src/os/linux/layout.f` first so `DLOPEN-SLOT` / `DLSYM-SLOT` are in scope, then
it dlopens `/usr/local/zed/lib/libsl_zed.so`, resolves the extern-"C"
`getZEDSDKRuntimeVersion_C(int*,int*,int*)`, calls it through `lib/ffi.f` `FFI-CALLN`
(the general-arity `ffi-call-n`), and prints `ZED SDK runtime version: 5.2.3`.
Non-invasive (a version query, no camera).

Validation observed `ZED SDK runtime version: 5 .2 .3` and stripped whitespace to
`5.2.3`. The SDK's dlopen spawns a CUDA background thread, and hb terminates its
main thread with `NR-EXIT` (not `exit_group`), so the process can linger after the
version line is flushed. Engine follow-up: hb should final-exit with `exit_group`
so FFI-spawned threads are reaped.

## Conventions (learned porting tegrastats)

The Habu locals/loop discipline shapes the port (see `docs/forth.md` and the
`habu-forth-gotchas` agent memory):

- Bind all locals before any `exit`; bind after closed `if/else` is fine.
- A `begin <cond> while <body> repeat` cond may only *add* a flag — peek in the
  cond, extract/consume in the body.
- Store parsed numbers as integers (e.g. tenths) to avoid float-in-`variable`
  storage, which would otherwise need trusted reinterpret casts.
- Floats parse with `lib/float.f` `STR>FLOAT`; output builds in the `lib/string.f`
  `SB` builder via `lib/fmt.f` (`SB-U`/`SB-INT`/`SB-FIX`).

Three traps the capture_schema port hit (all already covered in `docs/forth.md` —
re-read it before writing new Habu):

- **Stack signatures take TYPE KEYWORDS only** (`i64 n ptr u8 bool r a`), never
  descriptive names. `( node -- bool )` silently breaks the checker — `node` isn't
  a type, so a later word that wants `i64` mismatches and the error surfaces far
  away (e.g. "at 'then'"). Descriptive names go in the `{: :}` locals; a JSON node
  is `i64`, a JSON string is `ptr u8 i64`.
- **`n` accepts a `u8`; `i64` does not.** A `c@` byte fed to a word declared
  `( i64 -- )` fails "expected i64 actual u8". Declare byte-consuming params `n`.
- **`code`, like `i`/`j`/`k`, is a reserved word** (the `CODE` definer) and can't
  be a local. `true`/`false` are NOT defined — use `0 0=` / `0 0= 0=`, or define
  them once.
