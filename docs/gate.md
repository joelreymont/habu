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
as regular files, then run the native registry from that tree. The fixpoint
fixtures copy `bin/hb` through `TREE-COPY:FILE`, which refuses a symlink whose
target is outside the tree (`E-FS-PATH`).

Keep `HB_TMP` outside the checkout, under `TMPDIR` or another external scratch
directory. Absolute-load source-discovery and event-closure fixtures inherit
the checkout's source root when their scratch files are inside it; the same
fixtures pass unchanged with external scratch.

```sh
bin/hb --load test/run.f </dev/null
```

For definition-cache or persisted checker-state changes, also run the engine
edit matrix; this longer run is outside the native registry:

```sh
bin/hb --load test/compiler/native-build-edit-e2e.f -- qualify </dev/null
```

It runs the rebuilt engines, compares cached and cold executable/map pairs
after early, middle and late edits, and checks rejected input. Cold generations
can converge while edited cached products retain stale scratch values; this
matrix caught the checker's discarded `WAS` row cursor in saved DATA.

The registry runs only on a `tools/native-build.f` product. Its keyed images
include the fixture writer (`test/fixture-writer.f`), an application image
saved with the engine `src/lib/engine-candidate.f` resolves (an exported
`HABU_UNDER_TEST`, else the running engine), and `APP-IMAGE:SAVE` needs
`NATIVE-RUNTIME`, which only that build bakes. The engine `tools/bootstrap.sh`
installs is the recovery engine: it answers `using NATIVE-RUNTIME` with
`unknown package`, and its save of the writer stops with
`E-UNDEFINED habu: in save: undefined word 'NATIVE-RUNTIME:CAPTURE-PREPARE'`.
After a recovery, use that engine as `$HOST` above and gate its product. The
generation chain's fixpoint is likewise its last generation (`hb-b5` in the
directory `tools/two-generation-build.f` prints), never the seed it was given.

Multi-generation convergence is a separate check for codegen, self-hosting or
capture changes that can alter successive engine output; see
[bootstrap.md](bootstrap.md#generation-chain-check). The Gforth recovery checks
below are also separate from the registry.

### A green build does not show that the prefix boots from source

The build compiles the whole prefix with a checker: the host's up to
`src/core/check-hook.f`, then the window's own, which starts with the rows
the host recorded for everything before the hook (`TRANSFER-CHECKED`,
`src/core/checker.f`). An engine that boots its prefix from source judges
nothing before the hook: a signed `:` definition's declaration is its row,
without authority (on a cold boot, before `src/core/checker.f` claims the
source, the engine logs the declaration and the claim records it), and from
the claim on a `TRUSTED:` declaration's row carries authority; a `TRUSTED:`
or data word compiled before the claim has a row only from a `PRIM:` axiom. A
sealed boot marks a pre-hook word with no external row internal, and a checked
body naming it is refused `E-UNDEFINED`; an unsealed one
(`HABU_WHITEBOX_IMAGE=1`) binds the declared row
([forth.md](forth.md#rules-learned-by-refusal)). A prefix file loaded after the
hook that names a pre-hook word with no external row (a `PRIM:` axiom, or a
`TRUSTED:` declaration after the claim) therefore builds and then dies on every
sealed cold boot: with `PATH-CAP` named inside `TMP-PATH-CHECK` before it had a
row, the build printed `native-build OK` and the cold host died
`E-UNDEFINED habu: in tmp-path-check: undefined word 'PATH-CAP'`. The rows
cannot be withheld to make the build refuse it: without them the window cannot
compile `src/core/check-hook.f`'s first definition (the checker's reason, then
`ncomp: cannot compile REPORT-UNCHECKABLE`, throw 70).

After editing an ARM prefix file, boot the candidate's prefix cold before the
registry. This source-built route is ARM-only; Intel captures its checked
prefix through `tools/native-build.f`:

```sh
HB_TMP=$TMP HABU_AOT_GATE=1 bin/hb --load test/aot-wid-build.f
```

It ends `aot-wid-build: hb-pwid ready`. `bin/hb --load test/cold-naming-test.f`
is the focused check of the naming rule itself.

## Running the registry

`test/gate-stdlib-cases.f` is the registry. Each `SUITE` row runs its listed
files through the tree's `bin/hb`.

The `svg-pdf` row needs native librsvg (2.52 or newer) with Cairo PDF support,
plus `qpdf`, `pdfinfo` and `pdfimages` on PATH; on macOS,
`brew install librsvg qpdf poppler`. The other host requirements are in
[bootstrap.md](bootstrap.md#requirements).

The registry is noninteractive. Keep stdin at EOF: its child process groups
inherit that input, and a read from an inherited terminal stops them with
`SIGTTIN`. An agent runs the command without allocating a PTY.

On macOS, keep the machine awake with a process-scoped assertion:

```sh
sh -c 'caffeinate -is -w $$ & bin/hb --load test/run.f </dev/null'
```

The assertion process is a sibling of the engine, so the engine's child waits
cannot reap it; it exits when the enclosing shell ends. When timing the gate,
send `/usr/bin/time` output to stderr: on macOS, `time -o` passes its log
descriptor into the engine and its children, violating `process-image`'s
standard-descriptor-only fixture.

Sleep counts against the suites' elapsed-time deadlines: repeated sleep/wake
cycles produced simultaneous `TIMEOUT-UNDER-LOAD` reports with one suite left
in the pool. Check `pmset -g log` before treating that result as a hang, keep
the failed log, and rerun the unchanged candidate awake; never raise timeouts
or count the interrupted run as acceptance.

Run the gate at default priority. CPU time measures work at the speed of the
cores a row is given: at background priority, confined to the efficiency
cores, the unsealed engine build ran 351 s of CPU in 362 s without finishing,
against 77 s at default priority, and its row can meet the budget there.

### ARM-only rows

The Linux x86-64 engine has only tier 1: `0 set-tier` exits with
`set-tier: x86-64 runs tier 1 only`. The registry includes
`test/gate-arm-cases.f` only on an ARM host; its rows require the ARM host,
its source recovery, JIT, or tier 0, and its comments state each row's
reason.

Mixed rows, including `tier`, both compile-floor rows, and `outer-interpret`,
remain registered on x86-64. Their eligible tier-1 cases must run; a fixture
may guard an individual ARM-only premise after measuring the native refusal.

### Reading a red run

Failures print the suite label, exit outcome, and captured stdout and stderr.
A row the pool killed at its deadline, or one whose own deadline ended it with
an uncaught `E-PROC-TIMEOUT` (`GT-POOL-INNER-TIMEOUT?`, `test/gate-pool.f`),
reads `kind=TIMEOUT-UNDER-LOAD` with the pool's saturation at that moment.
Build and suite rows are held to a CPU budget (below), so the pool's deadline
is only a hang guard: its red line ends with the CPU time the row had run of
its budget (`cpu=<used>/<budget>ms`), and a row that ran little of it had
stopped running. A row the pool ended for its budget reads `kind=CPU-BUDGET`,
which no load explains.

A red that appears only when the box is loaded is the box: reproduce it on the
unmodified base under the same load, then rerun the suite alone. Run against a
matching private source and engine tree when the result must qualify that
exact pair.

### Scratch and cache

The run removes its temporary root whether it is green or red, so the printed
tail and the truncation line's byte count are what a finished red run leaves;
the capture file each `stdout-file:` line names is readable only while the run
is going. To keep a run's trees, give it its own `HB_TMP`: every maker (the
pool root, each spawned child's scratch, `hb-build`'s private build directory)
goes under it when it is set, and under `TMPDIR` or `/tmp` when it is not.
Every gate run gets its own `HB_TMP` root.

Keep a gate `HB_TMP` short enough that the deepest row path under it,
`streaming-sha256`'s deep file (`tools/sha256-file-test.f`), fits Habu's path
capacity `FS-PATH-CAP` (`src/lib/fs.f`), which `FS-PATHZ-INTO` enforces on every
file-system path on every host. `GT-START` (`src/lib/test/runner.f` `GT-ROOM-CHECK`) refuses only
an `HB_TMP` that leaves its own root no room; a row's deeper paths only this
rule keeps.

Each spawned child gets its own scratch directory as `HB_TMP`, removed when its
slot retires, and whatever the child or its children put there goes with it.
A row that puts its own `HB_TMP` in a child's environment owns and reaps that
directory. Unix-domain sockets go in the slot's `HB_SOCK_TMP`, a short
directory under `TMPDIR`: a socket path must fit `sun_path` (104 bytes on
macOS, 108 on Linux), and a nested row's `HB_TMP` does not.

Keyed images and native definition entries live in the selected build cache
(`src/lib/build-cache.f`). A checkout keeps one persistent build cache, the
default `BUILD-CACHE:ROOT$` or one fixed `HABU_BUILD_CACHE`, shared by every
gate run and every engine built from the checkout; every row gets it as
`HABU_BUILD_CACHE` (`test/gate-stdlib-lib.f` `SUITE-ENV`), so children and
nested gates reuse the same products across runs. The definition cache
(`tools/native-definition-cache.f`) stores one checksummed `hb-def-*.hbd` entry
per definition, keyed by what the definition was built from and the reading
checker and kernel. An entry that is short, has the wrong magic or fails its
checksum is refused with `native-cache: entry refused <path>: <reason>` on
stderr and compiled again. Normal retention removes products unused for a day;
hb-build prunes definition entries that way after each build whose children
open the definition cache. Select a fresh `HABU_BUILD_CACHE` only for a
deliberate cold measurement. Compiler bundles, grants, private whitebox copies
and mutable fixtures remain run-local. A killed keyed-image build's work
directory lasts only until the next keyed-image build in that cache
(`test/keyed-image-reap-test.f`).

### Signals and servers

The gate root answers SIGTERM, SIGINT and SIGHUP: it kills every live row,
removes its temporary root, and dies of the same signal. A signal the root was
started with ignored (`nohup`'s SIGHUP, a background job's SIGINT) stays
ignored. SIGKILL cannot be answered: what the rows spawned stays, with the
temporary root, the session's socket root under `TMPDIR` (`hb-sock-*`) and a
`pg` row's server, still running (`test/db/pg-cluster.f` has its recovery).
`test/gate-signal-test.f` checks each signal, a second signal during the
answer, `nohup`, and a row killed at its deadline.

A pool kill reaches every process descended from the row's child, not only its
process group (`src/lib/process-tree.f`). It cannot reach a daemon: a process
whose parent is gone and whose group names no member of the tree, as after
`setsid`. So a row starts a server as its own child, never through a launcher
that daemonizes it: the `pg` row runs `postgres`, not `pg_ctl`. A child that
catches SIGTERM is sent it first and given `GT-POOL-GRACE-MS` (10 s) to end
its whole tree; a SIGKILLed server runs no exit path, and PostgreSQL removes
its System V shared-memory segment only there, while `kern.sysv.shmmni` caps
segments host-wide (32 on the macOS hosts here). A child that does not catch
SIGTERM is killed without waiting.

## How a suite runs, and what that demands of its files

- A `SUITE` block is **one** `bin/hb --load` spawn: its files load into one
  image in order. A suite file is therefore package-scoped, duplicate-safe, and
  closes its `package` by its last line. `T-REPORT` returns after `test: ok`,
  so it may come before `;package`, as in the [forth.md](forth.md) example. A
  file that leaves its package open makes the next file that opens one die
  exit 75 at that `package` (`package at <file>:<line>`). Fixture identities
  carry the test's own tag so two suites never intern one name.
- Files in a `SUITE` row are entries unless an `ENTRIES` marker follows an
  inert preload prefix. The marker is not passed to `--load`; every file after
  it and before `--` is an entry. Put reusable definitions in preloads, then
  let each row run only its own assertions. Before any row starts, the gate
  walks the load graph (`test/gate-images.f`): from every file a row loads,
  through each import and each path literal a known load helper consumes, to
  every file those reach. It refuses a preload that is another row's entry and
  any import or launch of one, naming the file and line. A file may name
  itself, and tier twins share an entry. The shared `test/compiler/aot-mode.f`
  prefix selects a tier and is declared as a preload. Run
  `bin/hb --load test/gate-entry-guard-test.f` to check this boundary without
  starting the full suite.
- A `WHITEBOX-SUITE` row is the gate-only kind: the runner hands it the gate's
  private copy of the unsealed engine (`test/whitebox-engine.f`) because such a
  file reaches inside the engine, and the sealed `bin/hb` refuses those tokens
  — standalone such a file exits 70 with `hb: internal engine word: <TOKEN>`.
  Its checker is unsealed, so a checked body that names an internal word binds
  that word's recorded row and is checked against it
  ([forth.md](forth.md#rules-learned-by-refusal)): a whitebox suite calls an
  internal word from a plain `:` definition, and a wrong signature there is
  refused. A file that only *spawns* a child needing the unsealed engine is not
  of that kind: it names one itself through `test/whitebox-child.f`
  (`PROVIDE`, `ENGINE$`, `ENV!`) and stays a plain `SUITE`, green on its own.
- A row needs a keyed image when its load closure holds the image's module
  (`test/gate-images.f` lists the ten families and their build rows). Nothing
  is declared: load the module and the row waits for its build row. A failed
  build row is red with the builder's exit status and output, and every row
  that needs its image is red naming it; every other row runs. A process under
  the gate that settles an image its row was not granted (`HABU_GATE_IMAGES`,
  `test/image-grant.f`) — a child reached through a load the reader cannot see
  — dies with exit 69 naming the image instead of building it beside the build
  row. A row run on its own settles its own images. On a compiler bundle
  failure, the adapter forwards its complete captured stdout and stderr to the
  gate's corresponding streams before cleanup.
- A row is bounded by its own work, not by wall time, because the rows behind
  it wait on it and a saturated host stretches wall time. The pool ends a build
  or suite row as `kind=CPU-BUDGET` once its process tree has run `CPU-MS`,
  360 s of CPU, user and system (`test/suite-budget.f`; `test/gate-pool.f`
  `GT-POOL-CPU-BUDGET!`). Its pool deadline, `ROW-MS`, is a hang guard five
  times the budget and a minute, and the image builders and every process a
  long row starts take `CHILD-MS`, five times the budget. Beside 64 CPU-bound
  processes, at load averages up to 240, wall-time bounds ended eight of eleven
  long rows (the C2, build-fixpoint and native-build-entry rows) as
  `kind=TIMEOUT-UNDER-LOAD`; CPU bounds passed all eleven, which ran 699 s of
  CPU there against 680 s alone. Measure a row's CPU outside the gate, user
  plus system from `/usr/bin/time bin/hb --load <file>`. Reduce repeated work
  and share setup before splitting a slow row; the budget bounds runaway work,
  it does not set an acceptable cost per row.
- A child that runs a fraction of a second of CPU keeps a short deadline:
  native-build's smoke run (`tools/native-build-core.f` `SMOKE-TIMEOUT-MS`),
  build-fixpoint's boot check (`BF-BOOT-TIMEOUT-MS`), signing
  (`src/lib/codesign.f`), check.f's run stage (`tools/check-core.f`
  `CHK-DEADLINE-MS`), hb-build's makers (`tools/hb-build-lib.f`
  `HBB-MAKER-DEFAULT-MS`) and the runs of the images they build
  (`test/gate-common-lib.f` `GE-TIMEOUT-MS`). Beside 64 CPU-bound processes,
  at load averages up to 185, each ran in at most ten times its CPU time.
- A suite whose assertions depend on the compiler tier selects it itself.
  Only code compiled after `1 set-tier` belongs to the tier, so the line goes
  after the harness and tool requires (`src/lib/test.f`, the code-reading tools,
  test fixtures that drive the chain) and before the code under test; a
  library whose own words a case runs as the subject is required after it.
  `' W dup 4 + code-origin .` prints the tier that compiled `W`. No assertion
  depends on the harness's tier, and compiling it at tier 1 dominated these
  rows (`test/compiler/native-do.f`: 1.17 s with the line above its requires,
  0.13 s below them). `test/compiler/aot-mode.f` is only for a caller that runs
  one unchanged file at both tiers (the `*-aot` twin rows); a twin row lists
  the subject's harness before it, by the same rule.
- A row whose child compiles `src/habu/app-image.f` or the AOT linker only to
  reach its subject runs a keyed image with them already loaded: the saver
  image (`test/app-image-engine.f`: `PATH$`) or the linker image built on it
  (`test/preloaded-engine.f`: `LINKER$`, `LINKER-LOAD`). Their headers hold
  the rules: take the path before staging argv, start a program on the saver
  image with `1 set-tier`, link only subjects that reach no word the linker's
  load holds and none of its cells but claimed ones, and keep a row whose
  children run `ENGINE-CANDIDATE:PATH$` off the images. A row builds on the
  engine a subject whose closure reaches a word, or an unclaimed cell, of a
  module of the linker's lib closure the engine does not bake, and a subject
  whose claim is the engine's load order (`tools/hb-build-test-lib.f`
  `HBT-KEYED!`). A row whose claim is the saver's or the linker's own load
  (`test/app-image.f`) keeps compiling them from source. Ordinary builder
  children in the FILE, callback, stripped-entry and literal rows use
  `test/aot-builder-engine.f`, which starts the saved builder as `hb-build`
  does. A compile error in the saver's closure fails `app-image-build`, leaves
  the linker unbuilt and makes every row that loads either image red naming
  `app-image-build`; one in the linker's closure fails `linker-build` and the
  rows that load the linker.
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
- An uncaught throw in a child names its code on stderr and exits 67, unless
  the code is 1 to 255 or a refusal the checker rendered (70);
  [debugging.md](debugging.md) has the exit rule. The pool labels a row
  `TIMEOUT-UNDER-LOAD` when it exits 67 with its own report of `E-PROC-TIMEOUT`
  (-2502) as the last stderr line (`test/gate-pool.f` `GT-POOL-INNER-TIMEOUT?`).
- A deadline in a build or window child reaches the pool as a timeout through
  exit statuses, because a throw code cannot cross a process boundary.
  `tools/hb-build.f --engine`, `tools/native-build.f` and
  `tools/build-fixpoint.f` exit 124 (`PROC-TIMEOUT-RC`, `src/lib/process.f`)
  when a deadline expired. The ordinary application build CLI keeps exit 67;
  an engine build's ordinary exit 67 is not a timeout. `tools/native-build.f`
  and `tools/build-fixpoint.f` exit 74 for other failures they catch, after
  naming the throw code. build-fixpoint's `BF-RC0`, the build-fixpoint rows
  (`tools/build-fixpoint-test-lib.f` `BFT-FIXPOINT-RC`) and
  `test/whitebox-engine.f` throw `E-PROC-TIMEOUT` again on 124, and the rows
  rethrow it once the step is named. The window child
  (`test/native-window-owner-child.f`) exits 124 when its window load meets a
  deadline; its parents pass the program, the capture and the exit code to
  `SUBJECT:RC-TIMED-OUT` (`src/lib/test/subject.f`), which on 124 prints them
  and throws `E-PROC-TIMEOUT` again.
- A test's own capture deadline is no assertion result. `PROC-OUTCOME>RC`
  (`src/lib/process.f`), `GT-RC@` (`src/lib/test/runner.f`) and `GE-FAIL`
  (`test/gate-common-lib.f`) throw `E-PROC-TIMEOUT` for an expired deadline.
  The `src/lib/test/outcome.f` asserts that want an exit or a signal take the
  program and its captured stdout and stderr, and throw it through
  `T-TIMED-OUT`, which prints the case label, the program and the capture
  first. A test that MATCHes the outcome itself throws it from its `timeout`
  arm through `T-TIMED-OUT`, or through `SUBJECT:TIMED-OUT`
  (`src/lib/test/subject.f`) when it keeps no case label; both first print the
  program and the capture drained before the deadline, as `GE-FAIL` does for a
  gate entry. A capture inside a worker task only throws it, because a task
  asserts nothing, and the join throws it again
  (`src/lib/process-task-test.f`). So the row reaches the pool as
  `TIMEOUT-UNDER-LOAD` instead of failing an assertion on 137 or on a false
  exited flag. A deliberate kill is a `signaled` outcome and still reads
  128 + signal; a test that expects the deadline asserts it with
  `T-OUTCOME-TIMEOUT` or MATCHes it. A pty wait whose clock ends with the
  child still at the terminal throws it too (`src/lib/pty-harness.f`
  `WAIT-AFTER-WITHIN`), where a hang-up answers false; `test/proc-pty.f`, a
  child of the `engine-runtime-regressions` row, exits `PROC-TIMEOUT-RC` for it;
  `test/process-pty-tty-smoke.f` `DEADLINE` tears the supervised target down
  before it throws, because the linear handle cannot cross a catch.
  A named local wait or task join whose clock ends is the same deadline, never
  a `TTRUE` on a false flag: `TEST-WAIT:EXPIRED` (`src/lib/test/wait.f`)
  prints `<name>: a deadline expired` on stdout and throws it, and
  `TEST-WAIT:POLL` asks a predicate, pausing the task, until it answers true
  or the deadline expires (`src/lib/aio-test.f` `REACH`, `AWAIT-END`,
  `STOP-LOOP`). A worker task records its deadline and the main task throws it
  at the join. The wait keeps its deadline, retry count, sleep and grace
  period: a timeout row is never fixed by raising one.
- A build row's own capture deadline reaches the pool the same way. The image
  builders (`test/whitebox-engine.f`, `test/keyed-image.f`,
  `test/cold-engine.f`) and `test/aot-wid-build.f` read their captures through
  `PROC-OUTCOME>DEADLINE-RC`, which gives 124 where `PROC-OUTCOME>RC` throws.
  The image builders name the step and throw `E-PROC-TIMEOUT`;
  `test/aot-wid-build.f` dies with 124, and the rows that run it
  (`test/aot-wid-suite.f`, `test/aot-wide-format-lib.f`) throw
  `E-PROC-TIMEOUT` again on that status.
- A row linked on the keyed linker image (`test/preloaded-engine.f`
  `LINKER-LOAD`) reads its `-cases.f` child by exit status alone. A child
  whose checks start processes runs its entry through `GE-CHILD-RUN`
  (`test/gate-common-lib.f`): an `E-PROC-TIMEOUT` that escapes the checks ends
  the child with 124 and any other throw leaves with its own code;
  `LINKER-LOAD` names the row and throws `E-PROC-TIMEOUT` again on 124.
- A row that needs an external server starts a private one and stops it
  whatever its cases do. The `pg` row (`test/db/pg-cluster.f`) runs `initdb`
  and `postgres` from `PATH`, a gate requirement on every host
  ([bootstrap.md](bootstrap.md#requirements)); without them it fails naming
  the missing binary. The server is the harness's own child, so the pool's
  kill ends it with the row. It serves the cluster on a Unix-domain socket in
  the slot's `HB_SOCK_TMP` only, so concurrent rows share no port, and runs the
  cases in a child engine so a case that dies or hangs still leaves the harness
  to stop the server. A step past its own deadline ends the row as
  `kind=TIMEOUT-UNDER-LOAD` once the server has stopped.
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
