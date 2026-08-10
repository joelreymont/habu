# Bootstrap

`bin/hb` is generated and ignored. It is the only installed native build output.
It is the small stdin/TTY engine, not a snapshot launcher: core/checker/tool source
is loaded from the checkout at process start, and large dictionaries/checker
arenas live in runtime memory instead of being baked into the executable.
A checkout without `bin/hb` uses Gforth only to create private bootstrap
artifacts under `HB_TMP`; those artifacts exist only to produce `bin/hb`.

## Requirements

- macOS ARM64 or Linux AArch64.
- Linux gates require a working devpts setup: `/dev/ptmx`, `/dev/pts`, and PTY
  ioctls must be available to the user running the gate.
- Gforth with `{:` locals support. Homebrew `gforth` 0.7.3 is too old.
  A current Gforth snapshot such as `0.7.9_20260610` works.
- The Rocq proof assistant on `PATH`, version 9.2 or newer (`rocq --version`).
  The seven parity gates under `test/compiler/*-proof.f` compile `formal/` through
  `/usr/bin/env rocq` and assert what the run printed, so a host without it fails
  the gate outright. That is deliberate: the proofs are the only thing standing
  between the generated obligations and "nobody checked", and a gate that skipped
  itself on a host missing the toolchain would report green for a machine that
  proved nothing. Install it the way the host installs toolchains (`brew install
  rocq` on macOS) and re-run; do not add skip logic. Verify with
  `bin/hb --load test/compiler/ir-id-proof.f`, which must print `test: ok`.
- GB10 device gates (sm_121a) **require** the pinned 13.3 `ptxas` in
  `~/.habu/toolchain/ptxas-13.3.33`: since `habu-enforce-pinned-ptxas-4598a743`,
  an sm_121 assemble fails closed (`E-PTXTC-STALE`/`E-PTXTC-DIGEST`) unless the
  resolved assembler hashes to the pinned SHA-256 and reports version ≥ 13.3 — the
  older system CUDA 13.0 assembler (which costs ~27% GEMM throughput) is refused,
  not merely warned. Provisioning recipe (archive, sha256, install):
  `docs/codegen-verdict.md` "Pinned ptxas toolchain".

Verify the Gforth requirement:

```sh
tmp=$(mktemp)
printf ': f {: a :} a . cr ; 1 f bye\n' > "$tmp"
gforth "$tmp"
rm -f "$tmp"
```

That command must print `1` and exit zero. If the usable Gforth is not first on
`PATH`, set `GFORTH=/path/to/gforth`.

The preferred persistent local install path is `~/.local/bin/gforth`; verify it
with:

```sh
~/.local/bin/gforth --version
```

The known-good recovery version is `gforth 0.7.9_20260610`.

On macOS, a local snapshot build can be used as the recovery host without
installing it:

```sh
curl -L https://github.com/forthy42/gforth/archive/refs/tags/0.7.9_20260610.tar.gz -o gforth-0.7.9_20260610.tar.gz
tar -xzf gforth-0.7.9_20260610.tar.gz
cd gforth-0.7.9_20260610
./autogen.sh
UNSUITABLE_CC=none ./configure --prefix="$HOME/.local/gforth"
make -j"$(sysctl -n hw.ncpu)" gforth-itc gforth-light.fi
```

If `gforth-fast` is not installed, point `GFORTH` at a tiny wrapper around the
snapshot interpreter and image:

```sh
#!/bin/sh
exec /path/to/gforth-0.7.9_20260610/gforth-itc \
  -i /path/to/gforth-0.7.9_20260610/gforth-light.fi "$@"
```

`tools/bootstrap.sh` only requires the `GFORTH` command to pass the locals probe.
It does not require that Gforth was installed globally.

## No-Binary Recovery

```sh
HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth-or-wrapper tools/bootstrap.sh
```

The script defaults `HABU_TARGET` from the host (`macos-aarch64` or
`linux-aarch64`). Set `HABU_TARGET` explicitly only when the host cannot be
detected.

`tools/bootstrap.sh` does the whole recovery and installs exactly one file:
`bin/hb`.

1. validates that Gforth supports `{:` locals;
2. uses `test/nf.fs` and `bootstrap/` to create private bootstrap executables in
   `HB_TMP` from the same native source layers used by `tools/build-fixpoint.f`;
3. uses those private executables to produce the small stdin engine as `bin/hb`;
4. runs the normal `bin/hb` self-refresh so the installed binary is rebuilt from
   current source and reaches the byte-for-byte fixpoint.

The native refresh certifies each generated compiler payload, then loads it
through the build-only `--build` source mode. That mode does not apply the
ordinary pre-source friend seal; the generated payload owns a mandatory
`SEAL-FRIEND` boundary after the compiler prefix and before its driver. Normal
`--load`, stdin, baked-program, and REPL paths remain sealed before their first
user token.

The temporary files are not build products. The final installed `bin/hb` is the
native checked stdin/TTY engine rebuilt from current source.

## Periodic No-Binary Check

The normal native gate uses an existing `bin/hb`; it does not prove the
from-scratch Gforth recovery path. Run this periodic check after engine/compiler
changes:

```sh
tmp=$(mktemp -d "${TMPDIR:-/tmp}/habu-bootstrap-check.XXXXXX")
HABU_ALLOW_BOOTSTRAP=1 \
HABU_BOOTSTRAP_CHECK_ONLY=1 \
GFORTH="${GFORTH:-$HOME/.local/bin/gforth}" \
HB_TMP="$tmp" \
tools/bootstrap.sh
```

`HABU_BOOTSTRAP_CHECK_ONLY=1` builds the private Gforth/native bootstrap chain
through `hb-stdin` and exits before replacing `bin/hb`. Before returning, the
recovery-built engine runs the top-row hook subprocess regression against
itself; the missing compile-preflight path must exit 70 with empty stdout and
exactly `hb: compile preflight hook missing` plus one LF on stderr.

## DDC Audit (Diverse Double-Compiling)

`tools/ddc-verify.f` is the explicit (never per-commit) trust audit: it builds
`bin/hb` two independent ways and requires byte-identical output. A seed backdoor
would have to be mirrored in both the Gforth host and the native seed to survive
the sha256 compare, reducing seed trust to "no coordinated cross-host backdoor".
It is gated on `HABU_ALLOW_BOOTSTRAP=1`, like the launcher it drives.

The two chains and their comparison point:

- **Native chain** — the current `bin/hb`, which reproduces itself at the native
  fixpoint (`install --force` is byte-identical). This is the reference.
- **Gforth chain** — `tools/bootstrap.sh HABU_BOOTSTRAP_CHECK_ONLY=1` emits a raw
  seed engine `hb-stdin` via Gforth; the audit then runs the native fixpoint
  refresh on that seed (the exact `install --force` step the full recovery runs
  after `mv hb-stdin bin/hb`), re-targeted to a scratch engine path via
  `HABU_FIXPOINT_ENGINE` so the checkout's `bin/hb` is never replaced.

DDC compares **at the fixpoint**: the Gforth chain's refreshed engine must be
byte-identical to the native `bin/hb`. It does **not** diff the raw `hb-stdin`
seed directly. The raw seed is captured by a Gforth-lineage stage whose live REPL
sits at different absolute addresses than the native host, so its baked AOT-REPL
blob carries that host's `movz/movk` address immediates (currently ~542 `__text`
bytes plus the downstream code signature). `EM-SEED-AOT` re-relocates those bytes
at boot — they are dead yet host-dependent, so a raw seed-vs-fixpoint diff
diverges by design. The native fixpoint refresh re-captures the AOT blob from the
canonical small engine (identical layout regardless of Gforth-vs-native lineage),
which erases the dead host addresses; the two chains then converge byte-for-byte.

Ensure `bin/hb` is a fresh native fixpoint before the audit, then run it (the
Gforth chain needs `gforth` on `PATH` or `GFORTH` set, per Requirements above):

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install --force

HABU_ALLOW_BOOTSTRAP=1 GFORTH="${GFORTH:-$HOME/.local/bin/gforth}" bin/hb --load \
  lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f \
  tools/ddc-verify.f tools/ddc-drive.f
```

where `tools/ddc-drive.f` is a one-line `DDC-MAIN`. The tool prints
`ddc: byte-identical <sha>` and exits 0 on match, or `ddc: DIVERGENT` with both
digests, both lengths, and the first differing byte offset, and exits 1. It runs
for a few minutes (the Gforth chain dominates).

VERIFIED (2026-07-16): the two chains are byte-identical at the fixpoint. A
`DIVERGENT` verdict is a real finding — a genuine cross-host toolchain divergence
(a candidate coordinated seed backdoor, a non-deterministic emit, or a stale
`bin/hb` that was not refreshed to the fixpoint first). Investigate the first
differing offset before trusting either binary; do not paper over it in the tool.

## Refresh `bin/hb`

After `bin/hb` exists, do not use Gforth for normal work:

```sh
bin/hb --load tools/build-fixpoint-refresh.f -- install
```

`bin/hb --load` selects the host core/checker/env source prefix from the
running binary. Callers load only the libraries and tool source they need.

The `all`/`install` refresh is content-keyed. After a successful install the
tool writes a stamp — SHA-256 over the digests of `bin/hb`, the exact emitted
fixpoint and stdin stage sources captured at the moment the build consumed
them, and the whole ordered `require`/`include` closure of the native compiler
chain (`src/compiler/native/migrate.f`) — to `$HABU_FIXPOINT_STAMP` if set, else
`$XDG_CACHE_HOME/habu-fixpoint/stamp`, else `~/.cache/habu-fixpoint/stamp`.
A repeated refresh with an unchanged engine, unchanged stage sources and an
unchanged chain prints `fixpoint: cached <key-prefix>` and exits 0 without
rebuilding (~1s instead of ~7s). Because the stamp key includes the hash of the
current `bin/hb`, a replaced or stale engine can never false-skip: any byte
change to `bin/hb` or to a compiled stage source changes the key and forces the
full refresh. The chain is keyed separately because it reaches the stage engine
as prefix source read straight from the checkout rather than as emitted stage
bytes, so without that fold a chain edit would change no other stamp input; the
closure is walked once per refresh, before the build, and costs about 0.16s.
An edit to a file the chain does not load leaves the key alone.
Append `--force` to bypass the stamp and rebuild unconditionally; proof flows
(`tools/seed.f`, `tools/bootstrap.sh`) always pass `--force`. `-- all` only
writes the stamp when its product is byte-identical to `bin/hb`.

## Warm Dev Snapshot

For a hot edit/check loop, build a warm snapshot engine with the same command
but `-- snap`: it writes `$HB_TMP/hb-new`, a snapshot image that boots warm
(~0.02s vs ~0.07s) and checks user source at least as fast as `bin/hb`. The
snapshot build retires the image-writer/compiler tail before writing, so the
image carries only the dev surface (checker, stdlib prefix, REPL/debugger);
the checker stays fail-closed (a bad definition exits 70). `hb-new` is a
local dev artifact: it is never installed as `bin/hb`, never used as a gate
or candidate launcher, and must be rebuilt after source changes.

If a device tool (`maki/eval/device.f`, `maki/gpu.f`, `tools/ptx/*`) errors with a
cryptic missing-primitive name such as `ffi-call-abi`, the running `bin/hb` predates a
native FFI primitive — **refresh it with the command above.** The maki test suite
guards this with `maki/device-smoke.f`: it requires `lib/ffi-abi.f` (which fails closed
on a stale engine) and then runs a live `cuInit`/`cuDeviceGet` canary, so the break
surfaces early at the FFI layer.

Run the gate after bootstrap or refresh:

```sh
bin/hb --load test/run.f
```

This is the native port gate. It runs as a checked bounded DAG pool with
private `HB_TMP` roots. It proves the host `bin/hb`, source selection,
checker/lints, self-refresh, engine suite, REPL build, and AOT output for the
current platform. Host policy is argv, not env: append
`-- --pool-slots N --nested-pool-slots M` when running on a specific machine.
It intentionally does not run LLM benchmark fixtures or require JavaScript,
Python, Rust, TypeScript, or model runtimes.

The test suite runs directly in the small `bin/hb` engine; it does not bake a
top-level test-suite snapshot and it does not use checker/tool snapshot images
as launchers. Every ordinary run builds its `HABU_UNDER_TEST` candidate in
phase 15. Maker, artifact, and result caches remain, but none can skip that
phase. Snapshot coverage belongs to the native build/fixpoint path; generated
images are local artifacts and must not be committed.

### Performance

**The gate does not time itself.** It used to: each attempt was the whole gate,
measured against a fixed per-profile budget. That is gone. Timing the whole gate
meant every suite anyone landed permanently ate the budget's margin, so a tree
that had not regressed eventually failed on its own growth — and by the time it
was removed the gate was failing on every tree, blocking all landings, while
correctness stayed green (dot `habu-recalibrate-cold-gate-ec0ba309`).

Performance is judged only where a stopwatch wraps **one fixed workload and
nothing else**. Today that is the six confined JSON-reader benchmarks in
[`lib/json-read-perf-test.f`](../lib/json-read-perf-test.f), run by
[`test/json-read-perf-phase.f`](../test/json-read-perf-phase.f) as a single
quiescent fork after every scheduled phase has drained:

- **A verdict is a ratio, not a stopwatch reading.** A seventh slot in every
  round times a *frozen reference workload* that calls no code under test, and
  each benchmark is judged on its fastest sample divided by the reference's,
  against a recorded ratio and margin. A nanosecond budget is a claim about the
  machine that recorded it and needed a per-host calibration factor to survive
  meeting any other machine; a ratio needs none, because the machine appears in
  both terms and divides out. A benchmark over its ratio reds its own verdict
  and reds the gate through the ordinary red-phase path — the same exit status
  any failing phase produces.
- The reference is shaped like the work it normalises (a per-item call, a
  byte-at-a-time classify-and-copy, a teardown) and *not* like a stopwatch
  calibration spin. This was measured, not assumed: on an asymmetric-core host
  each loop shape has its own performance-to-efficiency core penalty, so a
  reference of the wrong shape does not cancel core placement, it compounds it.
- The phase brackets itself with a calibration spin before and after. That
  bracket no longer scales anything — it only asks whether the box held ONE
  speed while the rounds ran. A box that is merely slow is now fine, because
  the ratio cancels it; a box whose speed *moved* is **inadmissible** rather
  than a verdict: it is re-measured, and if the box never goes quiet the phase
  exits **68**, meaning *the measurement could not be taken* — never *the tree
  is slow*. Rerun in a quiet window. Every attempt records the one-minute load
  average and the runnable-process count on its evidence line.

Widening performance coverage means **adding another confined benchmark with its
own budget**, never re-introducing an aggregate timer over the whole gate.

Per-phase timeouts remain as hang guards, not perf verdicts.

`bin/hb` itself must stay the small source-loading engine.

## Future Port Checklist

1. Add one target source seam under `src/os/<target>/` for syscalls, executable
   layout, signing policy, terminal constants, and target metadata. Startup
   argv/envp access stays shared in `src/os/env-base.f`; do not add target
   `env.f` fallbacks.
2. Wire the target into `tools/bootstrap.sh`, `tools/build-fixpoint.f`, and the
   native source-list builders (`src/habu/habu2.f`, `bootstrap/cg/forth.fs`,
   `src/habu/stdin.f`, and `tools/hb-build-lib.f`) so bootstrap, refresh,
   `--load`, baked REPL, and AOT all select the same target prefix automatically
   from the running `bin/hb`.
3. Recover `bin/hb` only if needed with `HABU_ALLOW_BOOTSTRAP=1
   tools/bootstrap.sh`; after that, use only native `bin/hb`.
4. Run the refresh command above and require a byte-for-byte fixpoint.
5. Run the native port gate above on the target machine.
