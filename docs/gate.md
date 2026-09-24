# Native test suite

Build the engine the tree describes before running the suite — the product
image, from a tree copy, with a private copy of the current engine as the
host (the generation chain in [bootstrap.md](bootstrap.md#generation-chain-check)):

```sh
HABU_UNDER_TEST=$HOST HABU_FIXPOINT_ENGINE=$HOST HB_TMP=$TMP \
  $HOST --load tools/native-build.f -- $OUT
```

Copy the last generation to the tree's `bin/hb`, then run the one native
registry from that tree:

```sh
bin/hb --load test/run.f
```

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
files through the tree's `bin/hb`; there is no second test inventory.

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
gives that slot no directory (`test/nf-path-test.f` hands each build a root of
a chosen length; a pool path in front of it would overflow `NF-PATH-CAP`).

## How a suite runs, and what that demands of its files

- A `SUITE` block is **one** `bin/hb --load` spawn: its files load into one
  image in order. A suite file is therefore package-scoped, duplicate-safe, and
  closes its `package` before `T-REPORT`; a later suite otherwise dies exit 75
  with a bare token, one or two suites after the culprit. Fixture identities
  carry the test's own tag so two suites never intern one name.
- A `WHITEBOX-SUITE` row is the gate-only kind: the runner hands it the gate's
  private copy of the unsealed engine (`test/whitebox-engine.f`) because such a
  file reaches inside the engine, and the sealed `bin/hb` refuses those tokens
  — standalone such a file exits 70 with `hb: internal engine word: <TOKEN>`
  (measured on `test/whitebox-engine-suite.f`). A file that only *spawns* a
  child needing the unsealed engine is not of that kind: it names one itself
  through `test/whitebox-child.f` (`PROVIDE`, `ENGINE$`, `ENV!`) and stays a
  plain `SUITE`, green on its own.
- A suite whose assertions depend on the compiler tier selects it itself:
  `1 set-tier` before its requires, because only code compiled after that line
  belongs to the tier. The runner prepends nothing, so every row measures what
  `bin/hb --load <file>` measures. `test/compiler/aot-mode.f` is only for a
  caller that runs one unchanged file at both tiers (the `*-aot` twin rows).
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
