# Native test suite

Run focused tests through their real load paths first. Run the full native
registry when a change affects shared checker or compiler behavior, runtime
ABI, capture format or several libraries, or when focused tests cannot bound
its effects. Source location alone does not decide this. For behavior baked
into the engine, build a product candidate from a private copy of the matching
tree with a ready native engine as host:

```sh
HABU_UNDER_TEST=$HOST HABU_FIXPOINT_ENGINE=$HOST HB_TMP=$TMP \
  $HOST --load tools/native-build.f -- $OUT
```

Put that candidate at the private tree's `bin/hb`, then run the native registry
from that tree:

```sh
bin/hb --load test/run.f
```

The registry runs only on a `tools/native-build.f` product. Before its first
suite it saves the fixture writer (`test/fixture-writer.f`) as an application
image with the engine `lib/engine-candidate.f` resolves (an exported
`HABU_UNDER_TEST`, else the running engine), and `APP-IMAGE:SAVE` needs
`NATIVE-RUNTIME`, which only that build bakes. The engine `tools/bootstrap.sh`
installs is the recovery engine: it answers `using NATIVE-RUNTIME` with
`unknown package`, and a gate on it stops there with exit 70 and
`E-UNDEFINED habu: in save: undefined word 'NATIVE-RUNTIME:CAPTURE-PREPARE'`.
After a recovery, use that engine as `$HOST` above and gate its product. The
generation chain's fixpoint is likewise its last generation (`hb-b5` in the
directory `tools/two-generation-build.f` prints), never the seed it was given.

On macOS, keep the machine awake for the gate with a process-scoped assertion:

```sh
caffeinate -is bin/hb --load test/run.f
```

Sleep counts against the suites' elapsed-time deadlines. Repeated sleep/wake
cycles produced simultaneous `TIMEOUT-UNDER-LOAD` reports even with one suite
left in the pool. Check `pmset -g log` before treating that result as a hang,
retain the failed log, and rerun the unchanged candidate awake; do not raise
timeouts or count the interrupted run as acceptance.

`test/gate-stdlib-cases.f` is the registry. Each `SUITE` row runs its listed
files through the tree's `bin/hb`. Multi-generation convergence is a separate
check for codegen, self-hosting or capture changes that can alter successive
engine output; see [bootstrap.md](bootstrap.md#generation-chain-check). The
Gforth recovery checks below are also separate from this registry.

The gate runs executable checks without an external theorem prover. Passing it
establishes only the behavior exercised; see [proofs.md](proofs.md).

Failures print the suite label, exit outcome, and captured stdout and stderr.
The run removes its temporary root whether it is green or red — a red run used
to keep the whole tree, and `/tmp` filled with one root per red run — so the
printed tail and the truncation line's byte count are what a finished red run
leaves; the capture file each `stdout-file:` line names is readable only while
the run is still going. To keep a run's trees, give it its own `HB_TMP`: every
maker (the pool root, each spawned child's scratch, `hb-build`'s private build
directory) goes under it when it is set, and under `TMPDIR` or `/tmp` when it
is not.

The pool makes one scratch directory per spawned child and hands it to the
child as `HB_TMP`, then removes it when that slot retires — exited, signaled or
timed out. A child the pool kills cannot clean up after itself; the parent
does, and whatever the child (or its own children) put under `HB_TMP` goes with
the directory. An `HB_TMP` row the caller put in the child's environment itself
— a value other than the pool process's own, which `PROC-ENV-INHERIT-MISSING`
copies — is the caller's scratch to own and reap: the pool leaves the row and
gives that slot no directory.

## How a suite runs, and what that demands of its files

- A `SUITE` block is **one** `bin/hb --load` spawn: its files load into one
  image in order. A suite file is therefore package-scoped, duplicate-safe, and
  closes its `package` before `T-REPORT`; a later suite otherwise dies exit 75
  with a bare token, one or two suites after the culprit. Fixture identities
  carry the test's own tag so two suites never intern one name.
- Files in a `SUITE` row are entries unless an `ENTRIES` marker follows an
  inert preload prefix. The marker is not passed to `--load`; every file after
  it and before `--` is an entry. Put reusable definitions in preloads, then
  let each row run only its own assertions. Before the first fixture build, the
  gate compares canonical file identities across registered entries, preloads,
  source imports and path literals consumed by known load helpers. It reports
  a preload or source file that references another row's entry. The shared
  `test/compiler/aot-mode.f` prefix selects a tier and is declared as a preload.
  Run `bin/hb --load test/gate-entry-guard-test.f` to check this boundary
  without starting the full suite.
- A `WHITEBOX-SUITE` row is the gate-only kind: the runner hands it the gate's
  private copy of the unsealed engine (`test/whitebox-engine.f`) because such a
  file reaches inside the engine, and the sealed `bin/hb` refuses those tokens
  — standalone such a file exits 70 with `hb: internal engine word: <TOKEN>`
  (measured on `test/whitebox-engine-suite.f`). The gate builds that engine in
  the pool row `whitebox-engine-build` beside the other rows; a whitebox row
  reached before it retires waits for it. After a failed build every whitebox
  row is red with the build's exit status and points at that row's output,
  and the other rows keep running. A file that only *spawns* a
  child needing the unsealed engine is not of that kind: it names one itself
  through `test/whitebox-child.f` (`PROVIDE`, `ENGINE$`, `ENV!`) and stays a
  plain `SUITE`, green on its own.
- A suite whose assertions depend on the compiler tier selects it itself.
  Only code compiled after `1 set-tier` belongs to the tier, so the line goes
  after the harness and tool requires (`lib/test.f`, the code-reading tools,
  test fixtures that drive the chain) and before the code under test; a
  library whose own words a case runs as the subject is required after it
  (`test/compiler/native-exec.f` and `lib/array.f`). `' W dup 4 + code-origin .`
  prints the tier that compiled `W`. No assertion depends on the harness's
  tier, and compiling it at tier 1 dominated these rows
  (`test/compiler/native-do.f`: 1.17 s with the line above its requires,
  0.13 s below them). The runner prepends nothing, so every row measures what
  `bin/hb --load <file>` measures. `test/compiler/aot-mode.f` is only for a
  caller that runs one unchanged file at both tiers (the `*-aot` twin rows);
  a twin row lists the subject's harness before it, by the same rule.
- A row whose child compiles `src/habu/app-image.f` or the AOT linker only to
  reach its subject runs a keyed image with them already loaded
  (`test/preloaded-engine.f`: `APP-IMAGE$`, `LINKER$`, `LINKER-LOAD`); the gate
  settles both before its first fork. That file's header holds the rules: take
  the path before staging argv, start a program on the host with `1 set-tier`,
  link only subjects that require nothing in the linker's lib closure, and keep
  a row whose children run `ENGINE-CANDIDATE:PATH$` off the images. A row whose
  claim is the saver's or the linker's own load (`test/app-image.f`) keeps
  compiling them from source. `test/gate-aot-negative.f` and
  `test/stripped-address.f` run on `LINKER$`: their die paths are forks of the
  image, not `ENGINE-CANDIDATE` children, and the mapped-band fixture's first
  question to the process-map reader reads the running process's map, because
  every capture drops the map its builder read (`src/habu/proc-maps.f`).
  The accepted cost of settling both in
  `SUITE-SETUP`, as the fixture writer is: a compile error in the saver's or
  the linker's closure stops the gate at setup with the builder's diagnostic
  instead of failing the rows that load them.
- `bin/hb file.f` (no `--load`) drops to a REPL after a clean load and blocks
  on stdin — it looks like a hang, rc 124 under a timeout. Pipe `< /dev/null`,
  and give a spawned build child `/dev/null` stdin rather than letting it
  inherit one.
- A suite that throws through a linear `PROCESS-PTY` handle strands its gated
  target child, which holds the runner's capture pipe, and the runner waits
  forever (measured with `test/process-pty-io-smoke.f`). Pin such a refusal in
  the library's own suite, never in a gated smoke file.
- A CLI tool that reads the ambient argv (`SCRIPT-ARGV$`) is never `include`d
  into a shared image: it would read the harness's argv. Run it spawned, and
  keep its assertion in an argv-free `-test.f`.
- `SUBJECT:RUN` forks the live test process: call it with no package open — a
  forked child's `package X` is otherwise a nested-package reject, exit 75 —
  and never gate a CLI file that parses argv.
- An uncaught throw in a `--load` or spawned child exits with the throw
  code's low eight bits and prints nothing: exit 56 is `E-PROC-TRUNCATED`
  (-2504), 104 is `E-STR-BOUNDS` (-2200). Add multiples of 256 until a known
  `E-*` appears before guessing at the site.
- A red that appears only when the box is loaded is the box: reproduce it on
  the unmodified base under the same load, then rerun the suite alone. Every
  gate run gets its own `HB_TMP` root, and nothing edits the tree while a gate
  or a census is running.

## Separate Gforth recovery checks

Run `bin/hb --load test/nf-path-test.f` when changing the Gforth fixture's
scratch paths or build/run contract. This still executes the complete fixture,
including its Gforth child, but does not make every native suite pay for it.
The fixture supplies each build a root of a chosen length; a pool path in front
of it would overflow `NF-PATH-CAP`.

Run the [no-binary recovery check](bootstrap.md#periodic-no-binary-check) for
changes to the recovery seed, mirror, launcher or dependencies, or as an
explicit release recovery audit. The [DDC audit](bootstrap.md#ddc-audit-diverse-double-compiling)
is also explicit. Neither is part of the normal native registry or an
automatic per-commit or release gate.

If a private `XDG_CACHE_HOME` is used for a Gforth check, first check
`gforth -e bye` with that environment. A Gforth snapshot may reopen precompiled
`libcc-tmp` libraries from its cache: an empty private cache then fails before
any Habu fixture runs. Copy the installed Gforth cache into the private cache,
preserving symlinks, and verify Gforth starts before running the check.
