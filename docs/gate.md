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

Copy that candidate and its matching `hb.names` into the private tree's `bin/`
as regular files, then run the native registry from that tree. The fixpoint
fixtures copy `bin/hb` through `TREE-COPY:FILE`, which refuses a symlink whose
target is outside the tree (`E-FS-PATH`).

```sh
bin/hb --load test/run.f
```

A green native build does not show that the prefix boots from source. The
build compiles the whole prefix with a checker: the host's up to
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
([forth.md](forth.md#rules-learned-by-refusal)). A prefix
file loaded after the hook that names a pre-hook word with no external row (a
`PRIM:` axiom, or a `TRUSTED:` declaration after the claim) therefore builds
and then dies on every sealed cold boot - measured with `PATH-CAP`
inside `TMP-PATH-CHECK` before it had a row: `native-build OK`, then
`E-UNDEFINED habu: in tmp-path-check: undefined word 'PATH-CAP'` from the cold
host. The rows cannot be withheld to make the build refuse it: without them
the window cannot compile `src/core/check-hook.f`'s first definition (the
checker's reason, then `ncomp: cannot compile REPORT-UNCHECKABLE`, throw 70).
After editing an ARM prefix file, boot the candidate's prefix cold before the
registry. This source-built recovery route is ARM-only; Intel captures its
checked prefix through `tools/native-build.f`:

```sh
HB_TMP=$TMP HABU_AOT_GATE=1 bin/hb --load test/aot-wid-build.f
```

It ends `aot-wid-build: hb-pwid ready`. `bin/hb --load test/cold-naming-test.f`
is the focused check of the naming rule itself
([forth.md](forth.md#rules-learned-by-refusal)).

The registry runs only on a `tools/native-build.f` product. Its keyed images
(below) include the fixture writer (`test/fixture-writer.f`), an application
image saved with the engine `lib/engine-candidate.f` resolves (an exported
`HABU_UNDER_TEST`, else the running engine), and `APP-IMAGE:SAVE` needs
`NATIVE-RUNTIME`, which only that build bakes. The engine `tools/bootstrap.sh`
installs is the recovery engine: it answers `using NATIVE-RUNTIME` with
`unknown package`, and its save of the writer stops with
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

The Linux x86-64 engine has only tier 1: `0 set-tier` exits with
`set-tier: x86-64 runs tier 1 only`. The registry includes
`test/gate-arm-cases.f` only on an ARM host. Its nineteen whole rows require
the ARM host, its source recovery, JIT, or tier 0:

- `compiler-shadow`, `x86-64-link-records`, `aot-shadow-capture`: these fixtures
  use the ARM source window and observe x86-64 as a second target. On an x86-64
  host the resident shadow map is different (29 records instead of the six
  expected by `compiler-shadow`).
- `engine-stack-jit`: its direct return and loop-cell assertions inspect the
  ARM JIT stack model. On x86-64 those cell mutations return normally instead
  of raising the expected bounds refusal.
- `engine-stack-debugger`, `debugger-resume`: the ARM BRK debugger fixtures
  select tier 0; the x86-64 engine refuses that selection.
- `addrmap-call`: it decodes four-byte AArch64 `BL` instructions and reads the
  JIT address-map bitmap, after selecting tier 0.
- `prop`: its candidate compilation, untyped measurement, and false-reject
  oracle require the unchecked tier 0; the harness selects that tier before
  running any generated case.
- `build-fixpoint-sandbox`: both cases alter the ARM recovery assembler and
  install its source-built engine. Intel's native generation chain uses
  `tools/native-build.f`; its recovery cross-builds from ARM.
- `native-unit`, `native-unit-stale`: the version 1 NBR object profile carries
  AArch64 instructions and relocations. These rows execute its imported code.
  Source keys, artifact-format refusals and checked source admission remain
  in the shared `native-unit-refusals` row; Intel needs no keyed ARM export.
- `compiler-native-identity-spill`, `compiler-native-wide-frame`,
  `compiler-native-fused-moves`: these fixtures assert AArch64 instruction,
  register and frame layouts. Intel selection, allocation and emission have
  their own shared rows.
- `aot-chain-producer`, `aot-chain-capture`: their live-window mutation,
  capture and instruction-chain compaction checks need an ARM host. The
  producer builds a source-only recovery host. The capture format's inert
  metadata transfer, extent, capacity and refusal cases remain in the shared
  `aot-chain-storage` row.
- `compiler-native-code-span`, `compiler-aot-nested-body`: these inspect the
  ARM compact-blob planner, four-byte record trailers and ADR extent inference.
  Shared create/does and stripped-image rows cover the runtime behavior.
- `aot-seed-metadata`: its signed-image mutations target the ARM boot reader's
  packed ULEB record and site tables. `aot-seed-batch` still builds and runs the
  ordinary candidate on both targets.

Mixed rows, including `tier`, both compile-floor rows, and `outer-interpret`,
remain registered on x86-64. Their eligible tier-1 cases must run; a fixture
may guard an individual ARM-only premise after measuring the native refusal.

The gate runs executable checks without an external theorem prover. Passing it
establishes only the behavior exercised; see [proofs.md](proofs.md).

Failures print the suite label, exit outcome, and captured stdout and stderr.
A row the pool killed at its deadline, or one whose own deadline ended it with
an uncaught `E-PROC-TIMEOUT` (`GT-POOL-INNER-TIMEOUT?`, test/gate-pool.f),
reads `kind=TIMEOUT-UNDER-LOAD` with the pool's saturation at that moment.
Build and suite rows are held to a CPU budget (below), so the pool's deadline
is only a hang guard: its red line ends with the CPU time the row had run of
its budget, and a row that ran little of it had stopped running. A row the
pool ended for its budget reads `kind=CPU-BUDGET`, which no load explains.
The run removes its temporary root whether it is green or red — a red run used
to keep the whole tree, and `/tmp` filled with one root per red run — so the
printed tail and the truncation line's byte count are what a finished red run
leaves; the capture file each `stdout-file:` line names is readable only while
the run is still going. To keep a run's trees, give it its own `HB_TMP`: every
maker (the pool root, each spawned child's scratch, `hb-build`'s private build
directory) goes under it when it is set, and under `TMPDIR` or `/tmp` when it
is not. A spawned child's socket directory (below) goes under `TMPDIR` either
way.

A gate `HB_TMP` of at most 675 bytes keeps every row's paths within
`PATH-CAP` and Darwin's 1023-byte `PATH_MAX` on any host. The deepest row
path is `streaming-sha256`'s deep file (`tools/sha256-file-test.f`):
`<HB_TMP>/habu-native-suite-<ns>-<try>/pool-<pid>-<seq>-tmp/habu-sha256-file-test-<ns>-<try>/<230 bytes>/deep.bin`.
That is 295 fixed bytes, then `<ns>` twice, the mono-ns clock in decimal (14
digits before 27.8 hours of uptime, 15 before 11.6 days, 19 at most);
`<try>` twice, the temp-name attempt (under 64, so at most 2 digits);
`<pid>` (5 digits on Darwin, up to 7 on Linux); and `<seq>`, the pool's
spawn count (3 digits for this suite; the bound allows 4). At
those maxima it adds 348 bytes, and 1023 less 348 is 675. Measured on a host
with a 14-digit `<ns>` and a 5-digit `<pid>`, where it adds at most 333: a
685-byte root fit every row, and at 700 bytes that row was refused past
`PATH_MAX`. The runner checks only its own paths: `GT-START`
(`lib/test/runner.f`) refuses, once and by name before it makes anything, an
`HB_TMP` that leaves its root no room for a `GT-NAME-MAX` name within
`FS-PATH-CAP`, but a row's own deeper paths, like that one, only this bound
keeps.

The pool makes one scratch directory per spawned child and hands it to the
child as `HB_TMP`, then removes it when that slot retires — exited, signaled or
timed out. A child the pool kills cannot clean up after itself; the parent
does, and whatever the child (or its own children) put under `HB_TMP` goes with
the directory. A directory that will not go, such as a tree deeper than
`FS-PATH-CAP` that `REMOVE-TREE` refuses, is named with its row, path and code;
that row is red and the pool goes on, and the run's final cleanup names the
same refusal before the red exit. An `HB_TMP` row the caller put in the
child's environment itself — a value other than the pool process's own, which
`PROC-ENV-INHERIT-MISSING` copies — is the caller's scratch to own and reap:
the pool leaves the row and gives that slot no directory.

A keyed image's build (below) works in a directory of the build cache instead,
beside the keyed path, so its publish is one rename within the cache
(`BUILD-CACHE:WORK-OPEN`). The build holds that directory by a lock on it,
which the kernel drops only once the build and every child that inherited the
lock have exited, and before it makes its own it removes every such directory
that nothing holds. A directory left by a build that the pool, a signalled
gate, SIGKILL or a reboot killed therefore lasts only until the next
keyed-image build in that cache, and a live build's is never taken.
`test/keyed-image-reap-test.f` kills a build and then its builder beside a
live one.

Each spawned child also gets a short directory for Unix-domain sockets, handed
over as `HB_SOCK_TMP`. A socket's path has to fit `sun_path`, 104 bytes on
macOS and 108 on Linux; a gate row's `HB_TMP` is about 100 bytes long already
and grows with every pool that runs under a pool, while `TMPDIR` is the same at
every level. So the directories are made, one per spawned slot whoever owns its
`HB_TMP`, in one socket root per pool session under `TMPDIR`, and each goes
with its slot's scratch. A slot killed because the run is ending never retires:
its directories go as soon as it is killed. The socket root is in the cleanup
registry once, so a pool that ends with live slots and never kills them — an
uncaught throw, a `die` — still removes them all at exit; one entry per row
would not fit the registry's 64, which it keeps until a `CLEANUP-RUN`.

A slot the pool kills — at its deadline, or because the run is ending — is
first asked to end, if its child catches SIGTERM. A process that is SIGKILLed
runs none of its own exit path, and a server it ran loses what only that path
releases: PostgreSQL's System V shared-memory segment is removed there and
nowhere else, and `kern.sysv.shmmni` allows 32 on the macOS hosts here
([db.md](db.md#tests)). So the pool reads the
child's caught signals (`PROC-TREE:CATCHES?`, lib/process-tree.f) and, when
SIGTERM is among them, sends it and waits until the child has exited or
`GT-POOL-GRACE-MS` (10 s) has passed. Every child asked at the end of a run
gets that grace at once, while the others are killed. In that time the child
owes the pool its whole tree, since what it spawned goes to init when it
exits; the `pg` row's harness has `initdb` end the step it is in, or stops its
server and ends its case engine's tree, and kills an `initdb` or a server still
running six seconds on, which leaves its segment ([db.md](db.md#tests)).
A child that does not catch SIGTERM is not sent it: the default action would
end it at once and leave what it spawned beyond the walk. Its row is killed
without waiting, as before. A row's deadline that sends a SIGTERM holds the
pool's other rows for as long as the child takes to end, at most the grace;
the `pg` harness ends about 70 ms after its SIGTERM, measured with a backend
busy, or once `initdb`'s current step has ended.

The kill itself then reaches every process descended from the slot's child,
not only the child's process group. Every spawned child leads a group of its own
([process-pty.md](process-pty.md)), so a row's engine builds used to outlive the
row, reparented to init. `lib/process-tree.f` stops the tree before it lists it
and kills it once it has settled, or as it stands after two seconds. It follows
each member's children and the group each member leads, a zombie's too: a child
that forked and exited unreaped leaves what it forked in the group its pid still
names. Two things are beyond
its reach. A process whose parent is gone and whose group id names no member of
the tree, zombie included, as a daemon's after `setsid`: so a row starts a
server as its own child, never through a launcher that daemonizes it, as the
`pg` row runs `postgres` and not `pg_ctl` ([db.md](db.md#tests)). And on
macOS, a child whose spawn had not made it yet: a member counts as settled when none of its threads is runnable, so a
thread blocked in the kernel partway into a spawn - on a page-in, an allocation
or a lock - is not seen, and that spawn finishes after the kill. The walk's
repeated passes narrow that window; they do not close it.

The gate root answers SIGTERM, SIGINT and SIGHUP. At its next pool step it kills
every live row that way, removes its temporary root, and then dies of the same
signal, so its caller reads a death by that signal and not an exit. A signal the
root was started with ignored — `nohup`'s SIGHUP, the SIGINT of a background job
— stays ignored. SIGKILL cannot be answered: each row's reaper kills the row's
group, and what the rows spawned stays, with the temporary root and the
session's socket root under `TMPDIR` (`hb-sock-*`). A `pg` row's server is one
of what stays, still running; [db.md](db.md#tests) has its recovery. One that
lands during a tree walk also leaves what the walk had stopped stopped, not
running out its time. Measured on macOS: the kernel sends SIGHUP and then SIGCONT to a
stopped process when a death orphans its group, which ends it unless it ignores
SIGHUP; any other — one whose group was orphaned before it was stopped, or whose
parent lives on — stays stopped until something sends it SIGKILL or SIGCONT.
`test/gate-signal-test.f` runs the gate's own driver on a one-row registry and
checks each signal, a second signal during the answer, `nohup`, and a row killed
at its deadline.

## How a suite runs, and what that demands of its files

- A `SUITE` block is **one** `bin/hb --load` spawn: its files load into one
  image in order. A suite file is therefore package-scoped, duplicate-safe, and
  closes its `package` before `T-REPORT`; a later suite otherwise dies exit 75
  with a bare token, one or two suites after the culprit. Fixture identities
  carry the test's own tag so two suites never intern one name.
- Files in a `SUITE` row are entries unless an `ENTRIES` marker follows an
  inert preload prefix. The marker is not passed to `--load`; every file after
  it and before `--` is an entry. Put reusable definitions in preloads, then
  let each row run only its own assertions. Before any row or build row
  starts, the gate walks the load graph it derives the keyed images from
  (`test/gate-images.f`): from every file a row loads, through each import and
  each path literal a known load helper consumes, to every file those reach.
  It refuses a preload that is another row's entry and any import or launch of
  one, naming the file and line. A file may name itself, and tier twins share
  an entry. The shared `test/compiler/aot-mode.f` prefix selects a tier and is
  declared as a preload. Run `bin/hb --load test/gate-entry-guard-test.f` to
  check this boundary without starting the full suite.
- A `WHITEBOX-SUITE` row is the gate-only kind: the runner hands it the gate's
  private copy of the unsealed engine (`test/whitebox-engine.f`) because such a
  file reaches inside the engine, and the sealed `bin/hb` refuses those tokens
  — standalone such a file exits 70 with `hb: internal engine word: <TOKEN>`
  (measured on `test/whitebox-engine-suite.f`). Its checker is unsealed, so a
  checked body that names an internal word binds that word's recorded row and
  is checked against it ([forth.md](forth.md#rules-learned-by-refusal)): a
  whitebox suite calls an internal word from a plain `:` definition, and a
  wrong signature there is refused. That engine is a keyed image
  (below): its build row, `whitebox-engine-build`, also puts the copy in
  place, and every whitebox row waits for that row. A file that only *spawns*
  a child needing the unsealed engine is not of that kind: it names one itself
  through `test/whitebox-child.f` (`PROVIDE`, `ENGINE$`, `ENV!`) and stays a
  plain `SUITE`, green on its own; loading that file is what makes the row
  wait for the build row.
- The gate settles seven keyed images, each once per run in a pool row of its
  own started beside the first rows (`test/gate-images.f`): the fixture writer
  and the cold host it emits (`test/fixture-writer.f`, `test/cold-engine.f`;
  rows `fixture-writer-build`, `cold-engine-build`), the saver and linker
  images (`test/app-image-engine.f`, `test/preloaded-engine.f`;
  `app-image-build`, `linker-build`), the unsealed engine
  (`test/whitebox-engine.f`; `whitebox-engine-build`), the saved native
  builder (`test/saved-builder.f`; `saved-builder-build`) and the NBR package
  unit exported from the tree (`test/native-unit-image.f`;
  `native-unit-build`), a keyed file rather than an engine: a unit imports
  only into a build of the tree and by the engine that exported it, so an
  export and its import in one row would be two engine builds. A row needs an
  image when its load closure holds the image's module: the files it loads and
  what those import, or launch as a `.f` source through a path literal a known
  load helper consumes (`test/load-refs.f`, the reader the entry guard uses).
  Nothing is declared; a cold host needs the writer and the linker needs the
  saver image because their modules load those modules. A row whose images are
  not settled holds the registry until they are, while the rows before it keep
  running. A failed build row is red with the builder's exit status and
  output, and an image built on the failed one is not built: every row that
  needs either is red with a line naming the failed build row, and every other
  row runs. Each row's environment names its images in `HABU_GATE_IMAGES`
  (`test/image-grant.f`): a process under the gate that settles an image its
  row was not granted — a child reached through a load the reader cannot see —
  dies with exit 69 naming the image instead of building it beside the build
  row. A row run on its own has no such variable and settles its own images.
- A build row is bounded by its own work, not by wall time, because the rows
  behind it wait on it and a saturated host stretches wall time. On an Apple
  M2 Max (eight performance and four efficiency cores) the unsealed engine
  build ran 77 s of CPU in 214 s at load average 93; beside 64 CPU-bound
  processes, a gate run of one whitebox row ran 77 s of CPU in all while its
  build row took 449 s. The pool reads the CPU time, user and system, of the
  row's process and every live process descended from it, with what each
  reaped (`lib/process-tree.f` `CPU-NS`), at most once a second, and ends the
  row as `kind=CPU-BUDGET` once it has run the budget every gate row has
  (`test/suite-budget.f` `CPU-MS`, 360 s; `test/gate-pool.f`
  `GT-POOL-CPU-BUDGET!`). Its wall deadline is only a hang guard, sized for a
  saturated pool: every builder's capture deadline is five times the budget
  (`CHILD-MS`) and the row's a minute more (`ROW-MS`), so a build within its
  budget meets it only on less than a fifth of a core. A build that stopped
  running gains no CPU time and still ends there, as
  `kind=TIMEOUT-UNDER-LOAD`; its red line ends with the CPU time the row had
  run of its budget (`cpu=<used>/<budget>ms`). CPU time measures work at the
  speed of the cores a build is given, so run the gate at default priority: at
  background priority, confined to the efficiency cores, the same build ran
  351 s of CPU in 362 s and had not finished, and its row can meet the budget
  there.
- A suite row is bounded the same way (`test/suite-budget.f`): the pool ends
  it as `kind=CPU-BUDGET` once its tree has run `CPU-MS`, 360 s of CPU, and
  its deadline in the pool, `ROW-MS`, is a hang guard five times that and a
  minute (`test/gate-stdlib-lib.f` `ROW-BUDGET!`). A long row gives each
  process it starts `CHILD-MS`, five times the budget, as its deadline: the C2
  acceptance rows (`test/c2-*-e2e.f` but `c2-init-accessor-e2e.f`, whose three
  children run 4 s of CPU in all against 120 s each),
  `test/native-build-entry.f`, the build-fixpoint rows
  (`tools/build-fixpoint-test-lib.f` `BFT-TIMEOUT-MS`),
  `test/native-window-owner.f`, `test/app-image.f` and
  `test/field-proj-boundary.f`, whose tier-1 windows run 20 to 27 s of CPU.
  The aot-positive rows give it to their maker child
  (`test/gate-aot-positive-lib.f` `MAKER-RUN`, 6 s of CPU); their other
  children keep shorter deadlines (below). Alone, the longest rows
  run 61 to 113 s of CPU, c2-memory the most. Beside 64 CPU-bound processes,
  at load averages up to 240, the gate's driver ran eleven of these rows (the
  C2, build-fixpoint and native-build-entry rows) both ways. Bounded by wall
  time, eight ended as `kind=TIMEOUT-UNDER-LOAD`: five C2 rows and
  build-fixpoint-snapshot at the row's 360 s, native-build-entry and
  build-fixpoint-fixtures at their children's 180 s and 120 s. Bounded by CPU
  time, all eleven passed, c2-memory last at 1088 s, and they ran 699 s of CPU
  there against 680 s alone. A row's CPU is measured outside the gate, user
  plus system from `/usr/bin/time bin/hb --load <file>`, and stays under half
  the budget (`test/gate-stdlib-cases.f`). A short child keeps a short deadline:
  native-build's smoke run (`tools/native-build-core.f` `SMOKE-TIMEOUT-MS`,
  10 s) and build-fixpoint's boot check of a candidate (`BF-BOOT-TIMEOUT-MS`,
  30 s) each run about 20 ms of CPU, and on a freshly signed copy of the
  engine each ended within 0.1 s beside those 64 processes, its signing
  (`lib/codesign.f`, 10 s) within 0.25 s, so load does not reach those
  deadlines. Nor does it reach compile-floor-gate's 180 s, whose tools run
  about a second of CPU each, check.f's run stage (`tools/check-core.f`
  `CHK-DEADLINE-MS`, 120 s), where no check.f a gate row starts runs a second,
  hb-build's makers in the aot-positive rows (`tools/hb-build-lib.f`
  `HBB-MAKER-DEFAULT-MS`, 600 s), 6 s of CPU each, or the runs of the images
  they build (`test/gate-common-lib.f` `GE-TIMEOUT-MS`, 120 s), a fraction of
  a second: beside 64 CPU-bound processes, at load averages up to 185, each
  ran in at most ten times its CPU time, a maker in up to 53 s, the tools in
  9 s and a check.f in 7 s.
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
  reach its subject runs a keyed image with them already loaded: the saver
  image (`test/app-image-engine.f`: `PATH$`) or the linker image built on it
  (`test/preloaded-engine.f`: `LINKER$`, `LINKER-LOAD`). Their headers hold
  the rules: take the path before staging argv, start a program on the saver
  image with `1 set-tier`, link only subjects that reach no word the linker's
  load holds and none of its cells but claimed ones, and keep a row whose
  children run `ENGINE-CANDIDATE:PATH$` off the images. That load ran before
  the capture window opens, so its modules, names and cells are not the ones a
  production build carries: a maker on the linker image refuses a closure
  reaching one of its words by name, or one of its cells that no AOT ownership
  claim names, and a subject defining one of its words dies at that line, as it
  dies on the engine at the library's line (rule 3).
  A row builds on the engine a subject whose closure reaches a word, or an
  unclaimed cell, of a module of the linker's lib closure the engine does not
  bake, which the image refuses by name, and a subject whose claim is the
  engine's load order (`tools/hb-build-test-lib.f` `HBT-KEYED!`). A subject
  that only requires such a module links on `LINKER$`, and so does one that
  reaches nothing of the image's load but cells a claim carries:
  `test/stripped-preloaded-runtime.f` links there a subject sharing the
  engine-baked FFI and TASK modules' claimed cells. A row whose claim is the
  saver's or the linker's own load (`test/app-image.f`) keeps compiling them
  from source.
  `test/gate-aot-negative.f` and `test/stripped-address.f` run on `LINKER$`:
  their die paths are forks of the image, not `ENGINE-CANDIDATE` children, and
  the mapped-band fixture's first question to the process-map reader reads the
  running process's map, because every capture drops the map its builder read
  (`src/habu/proc-maps.f`). A compile error in the saver's closure fails
  `app-image-build`, leaves the linker unbuilt and makes every row that loads
  either image red naming `app-image-build`; one in the linker's closure fails
  `linker-build` and the rows that load the linker. Every other row runs.
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
- A deadline in a build child reaches the pool as a timeout through exit
  statuses, because a throw code cannot cross a process boundary.
  `tools/native-build.f` and `tools/build-fixpoint.f` exit 124
  (`PROC-TIMEOUT-RC`, `lib/process.f`) when a deadline expired and 74 for any
  other failure they catch, after naming its throw code. build-fixpoint's
  `BF-RC0`, the build-fixpoint rows (`tools/build-fixpoint-test-lib.f`
  `BFT-FIXPOINT-RC`) and `test/whitebox-engine.f` throw `E-PROC-TIMEOUT` again
  on 124, and the rows rethrow it once the step is named.
- A test's own capture deadline is no assertion result. `PROC-OUTCOME>RC`
  (`lib/process.f`), `GT-RC@` (`lib/test/runner.f`) and `GE-FAIL`
  (`test/gate-common-lib.f`) throw `E-PROC-TIMEOUT` for an expired deadline.
  The `lib/test/outcome.f` asserts that want an exit or a signal take the
  program and its captured stdout and stderr, and throw it through
  `T-TIMED-OUT`, which prints the case label, the program and the capture
  first. A test that MATCHes the outcome itself throws it from its `timeout`
  arm through `T-TIMED-OUT`, or through `SUBJECT:TIMED-OUT`
  (`lib/test/subject.f`) when it keeps its own counters (`test/proc-pty.f`);
  both first print the program and the capture drained before the deadline,
  as `GE-FAIL` does for a gate entry. A capture inside a worker task only
  throws it, because a task asserts nothing, and the join throws it again
  (`lib/process-task-test.f`). So the row reaches the pool as
  `TIMEOUT-UNDER-LOAD` instead of failing an assertion on 137 or on a false
  exited flag. A deliberate kill is a `signaled` outcome and
  still reads 128 + signal; a test that expects the deadline asserts it with
  `T-OUTCOME-TIMEOUT` or MATCHes it. A pty wait whose clock ends with the
  child still at the terminal throws it too (`lib/pty-harness.f`
  `WAIT-AFTER-WITHIN`), where a hang-up answers false; `test/proc-pty.f`, a
  child of the `engine-runtime-regressions` row, exits `PROC-TIMEOUT-RC` for it;
  `test/process-pty-tty-smoke.f` `DEADLINE` tears the supervised target down
  before it throws, because the linear handle cannot cross a catch.
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
  `test/gate-aot-negative-cases.f` and `test/stripped-address-cases.f` do so;
  `test/compiler/native-code-span-cases.f` and
  `test/compiler/aot-nested-body-cases.f` start no process and have no
  deadline to report.
- A row that needs an external server starts a private one and stops it
  whatever its cases do. The `pg` row (`test/db/pg-cluster.f`) runs `initdb`
  and `postgres` from `PATH`, a gate requirement on every host
  ([bootstrap.md](bootstrap.md#requirements)); without them it fails naming
  the missing binary. The server is the harness's own child, so the pool's
  kill ends it with the row. It serves the cluster on a Unix-domain socket
  only, so concurrent rows share no port, and runs the cases in a child engine
  so a case that dies or hangs still leaves the harness to stop the server.
  A step past its own deadline ends the row as `kind=TIMEOUT-UNDER-LOAD` once
  the server has stopped ([db.md](db.md#tests)).
  The socket directory is the slot's `HB_SOCK_TMP` rather than a directory
  under its `HB_TMP`, whose length leaves no room in `sun_path`.
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
