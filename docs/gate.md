# Native test suite

Run focused tests through their real load paths first. Run the full native
registry when a change affects shared checker or compiler behavior, runtime
ABI, capture format or several libraries, or when focused tests cannot bound
its effects. Source location alone does not decide this.

## Focused tests

Run a test file directly with `bin/hb --load`, naming on one command line the
library files the engine does not bake ahead of it, as the test's header
states; never concatenate files with shell logic:

```sh
bin/hb --load src/lib/errors.f src/lib/process.f test/proc-pty.f
```

The runner prepends nothing to a registry row, so a focused run of a row's
files measures what the row measures.

## Qualifying a candidate

For behavior baked into the engine, build a product candidate from a private
copy of the matching tree with a ready native engine as host:

```sh
HABU_UNDER_TEST=$HOST HABU_FIXPOINT_ENGINE=$HOST HB_TMP=$TMP \
  $HOST --load tools/native-build.f -- $OUT
```

Copy that candidate and its matching `hb.map` into the private tree's `bin/`
as regular files, not symlinks, then run the native registry from that tree.
Keep `HB_TMP` outside the checkout, under `TMPDIR` or another external scratch
directory.

```sh
bin/hb --load test/run.f </dev/null
```

For definition-cache or persisted checker-state changes, also run the engine
edit matrix; this longer run is outside the native registry:

```sh
bin/hb --load test/compiler/native-build-edit-e2e.f -- qualify </dev/null
```

The registry runs only on a `tools/native-build.f` product. The engine
`tools/bootstrap.sh` installs is the recovery engine and cannot run it: after a
recovery, use that engine as `$HOST` above and gate its product. The
generation chain's fixpoint is likewise its last generation, never the seed it
was given.

Multi-generation convergence is a separate check for codegen, self-hosting or
capture changes that can alter successive engine output; see
[bootstrap.md](bootstrap.md#generation-chain-check). The Gforth recovery checks
below are also separate from the registry.

### A green build does not show that the prefix boots from source

The build's checker holds the rows its host recorded for the prefix before the
check hook; an engine that boots the prefix from source does not, so a prefix
file can build and then fail every sealed cold boot
(`test/cold-naming-test.f` states the rule). After editing an ARM prefix file,
boot the candidate's prefix cold before the registry. This source-built route
is ARM-only; Intel captures its checked prefix through `tools/native-build.f`:

```sh
HB_TMP=$TMP HABU_AOT_GATE=1 bin/hb --load test/aot-wid-build.f
```

It ends `aot-wid-build: hb-pwid ready`. `bin/hb --load test/cold-naming-test.f`
is the focused check of the naming rule itself.

## Running the registry

`test/gate-stdlib-cases.f` is the registry. Each `SUITE` row runs its listed
files through the tree's `bin/hb`. Host requirements are in
[bootstrap.md](bootstrap.md#requirements).

The registry is noninteractive. Keep stdin at EOF: its child process groups
inherit that input, and a read from an inherited terminal stops them with
`SIGTTIN`. An agent runs the command without allocating a PTY.

On macOS, keep the machine awake with a process-scoped assertion:

```sh
sh -c 'caffeinate -is -w $$ & bin/hb --load test/run.f </dev/null'
```

When timing the gate, send `/usr/bin/time` output to stderr: on macOS,
`time -o` passes its log descriptor into the engine and its children, which
the standard-descriptor fixtures refuse.

Sleep counts against the suites' deadlines. Check `pmset -g log` before
treating a timeout as a hang, keep the failed log, and rerun the unchanged
candidate awake; never raise timeouts or count the interrupted run as
acceptance.

Run the gate at default priority: rows are held to a CPU budget, and at
background priority a row confined to efficiency cores can exceed it.

### ARM-only rows

The registry includes `test/gate-arm-cases.f` only on an ARM host; its rows
need the ARM host, its source recovery or tier 0, and its comments state each
row's reason. Mixed rows stay registered on x86-64, where their tier-1 cases
must run; a fixture may guard an individual ARM-only premise after measuring
the native refusal.

### Reading a red run

Failures print the suite label, exit outcome, and captured stdout and stderr.
`kind=TIMEOUT-UNDER-LOAD` is a row that met a deadline; its line ends with the
CPU time the row had run of its budget, and a row that ran little of it had
stopped running. `kind=CPU-BUDGET` is a row that ran out of its budget, which
no load explains.

A red that appears only when the box is loaded is the box: reproduce it on the
unmodified base under the same load, then rerun the suite alone. Run against a
matching private source and engine tree when the result must qualify that
exact pair.

### Scratch and cache

The run removes its temporary root whether it is green or red, so the printed
tail is what a finished red run leaves. To keep a run's trees, give it its own
`HB_TMP`: every maker goes under it when it is set, and under `TMPDIR` or
`/tmp` when it is not. Every gate run gets its own `HB_TMP` root.

A checkout keeps one persistent build cache for keyed images and native
definition entries, the default or one fixed `HABU_BUILD_CACHE`, shared by
every gate run and every engine built from the checkout. Select a fresh
`HABU_BUILD_CACHE` only for a deliberate cold measurement.

### Signals and servers

The gate root answers SIGTERM, SIGINT and SIGHUP: it kills every live row,
removes its temporary root, and dies of the same signal. SIGKILL cannot be
answered: what the rows spawned stays, with the temporary root, the session's
socket root under `TMPDIR` (`hb-sock-*`) and a `pg` row's server, still
running (`test/db/pg-cluster.f` has its recovery).

A pool kill reaches every process descended from the row's child, but not a
daemon. So a row starts a server as its own child, never through a launcher
that daemonizes it: the `pg` row runs `postgres`, not `pg_ctl`.

## How a suite runs, and what that demands of its files

- A `SUITE` block is **one** `bin/hb --load` spawn: its files load into one
  image in order. A suite file is therefore package-scoped, duplicate-safe, and
  closes its `package` by its last line. `T-REPORT` returns after `test: ok`,
  so it may come before `;package`, as in the [forth.md](forth.md) example.
  Fixture identities carry the test's own tag so two suites never intern one
  name.
- Files in a `SUITE` row are entries unless an `ENTRIES` marker follows an
  inert preload prefix. Put reusable definitions in preloads, then let each row
  run only its own assertions. The gate refuses a preload that is another row's
  entry and any import or launch of one (`test/gate-images.f`);
  `bin/hb --load test/gate-entry-guard-test.f` checks this without starting
  the full suite.
- A `WHITEBOX-SUITE` row runs on the gate's unsealed engine
  (`test/whitebox-engine.f`), because its file reaches inside the engine and
  the sealed `bin/hb` refuses that. A file that only spawns a child needing the
  unsealed engine names one itself through `test/whitebox-child.f` and stays a
  plain `SUITE`.
- A row needs a keyed image when its load closure holds the image's module;
  nothing is declared. A failed build row makes every row that needs its image
  red naming it; every other row runs.
- A row is bounded by its own CPU work, not by wall time
  (`test/suite-budget.f`). Measure a row's CPU outside the gate, user plus
  system from `/usr/bin/time bin/hb --load <file>`. Reduce repeated work and
  share setup before splitting a slow row; the budget bounds runaway work, it
  does not set an acceptable cost per row.
- A suite whose assertions depend on the compiler tier selects it itself, with
  `1 set-tier` after the harness and tool requires and before the code under
  test, so the harness is not compiled at tier 1. `test/compiler/aot-mode.f` is
  only for the `*-aot` twin rows that run one unchanged file at both tiers.
- A row whose child compiles `src/habu/app-image.f` or the AOT linker only to
  reach its subject runs a keyed image with them already loaded:
  `test/app-image-engine.f` or `test/preloaded-engine.f`, whose headers hold
  the rules.
- `bin/hb file.f` (no `--load`) drops to a REPL after a clean load and blocks
  on stdin — it looks like a hang, rc 124 under a timeout. Pipe `< /dev/null`,
  and give a spawned build child `/dev/null` stdin rather than letting it
  inherit one.
- A suite that throws through a linear `PROCESS-PTY` handle strands its gated
  target child and the runner waits forever. Pin such a refusal in the
  library's own suite, never in a gated smoke file.
- A CLI tool that reads the ambient argv is never `include`d into a shared
  image: it would read the harness's argv. Run it spawned, and keep its
  assertion in an argv-free `-test.f`.
- `SUBJECT:RUN` forks the live test process: call it with no package open, and
  never gate a CLI file that parses argv.
- A deadline is never an assertion result. A test's expired wait, capture or
  join throws `E-PROC-TIMEOUT` and a child reports it as exit 124, so the row
  reaches the pool as `TIMEOUT-UNDER-LOAD`; a test that expects the deadline
  asserts it. A timeout row is never fixed by raising a deadline, retry count
  or sleep. [debugging.md](debugging.md) has the exit rule.
- A row that needs an external server starts a private one as its own child,
  on a Unix-domain socket in the slot's `HB_SOCK_TMP`, and stops it whatever
  its cases do (`test/db/pg-cluster.f`).
- A row earns its place by a user-visible failure no other row catches. A
  refusal of in-memory state no real input reaches (a hand-built row or
  table, a forged token, a patched code byte) is not one: the real path runs
  in every build, and the test pins internal APIs. A capacity limit a real
  program can reach is one, even when the test reaches it by a shortcut, and
  so is a refusal of a malformed saved file (image, cache unit, effect
  graph); such a test corrupts the file on disk rather than steering the
  producer through a test-only switch. Platform rows are switched by
  platform, never trimmed.

## Separate Gforth recovery checks

Run `bin/hb --load test/nf-path-test.f` when changing the Gforth fixture's
scratch paths or build/run contract. This still executes the complete fixture,
including its Gforth child, but does not make every native suite pay for it.

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

## Verification claims

The gate runs executable checks; passing it establishes only the behavior
exercised, not a formal soundness or refinement theorem.

- A published claim must constrain the implementation, rather than merely
  restate its reference model. Use a counterexample when needed to establish
  that a check detects the defect it claims to catch.
- A result that restates a model's own definition establishes no implementation
  behavior. Test the production path instead.
- State assumptions, unsupported cases and known counterexamples.
- An unproved claim remains unproved. Record the limitation where the claim is
  documented; never weaken it silently until it passes.
- Tests and measurements establish different properties. Describe what was
  actually checked without mandatory mutation campaigns or extra ledgers.
