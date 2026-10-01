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

A green native build does not show that the prefix boots from source. The
build compiles the whole prefix with a checker: the host's up to
`src/core/check-hook.f`, then the window's own, which starts with the rows
the host recorded for everything before the hook (`TRANSFER-CHECKED`,
`src/core/checker.f`). An engine that boots its prefix from source has no
checker before the hook and only axiom rows after it. A prefix file loaded
after the hook that names a pre-hook word without a `PRIM:` row therefore
builds and then dies on every cold boot - measured with `PATH-CAP` inside
`TMP-PATH-CHECK` before it had a row: `native-build OK`, then
`E-UNDEFINED habu: in tmp-path-check: undefined word 'PATH-CAP'` from the cold
host. The rows cannot be withheld to make the build refuse it: without them
the window cannot compile `src/core/check-hook.f`'s first definition
(`ncomp: cannot compile REPORT-UNCHECKABLE`, `E-NCOMP-ARITY`). After editing a
prefix file, boot the candidate's prefix cold before the registry:

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

The gate runs executable checks without an external theorem prover. Passing it
establishes only the behavior exercised; see [proofs.md](proofs.md).

Failures print the suite label, exit outcome, and captured stdout and stderr.
A row the pool killed at its deadline, or one whose own deadline ended it with
an uncaught `E-PROC-TIMEOUT` (`GT-POOL-INNER-TIMEOUT?`, test/gate-pool.f),
reads `kind=TIMEOUT-UNDER-LOAD` with the pool's saturation at that moment: still
red, but a deadline missed on a loaded host rather than a defect.
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
`PATH_MAX`.

The pool makes one scratch directory per spawned child and hands it to the
child as `HB_TMP`, then removes it when that slot retires — exited, signaled or
timed out. A child the pool kills cannot clean up after itself; the parent
does, and whatever the child (or its own children) put under `HB_TMP` goes with
the directory. An `HB_TMP` row the caller put in the child's environment itself
— a value other than the pool process's own, which `PROC-ENV-INHERIT-MISSING`
copies — is the caller's scratch to own and reap: the pool leaves the row and
gives that slot no directory.

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
  (measured on `test/whitebox-engine-suite.f`). That engine is a keyed image
  (below): its build row, `whitebox-engine-build`, also puts the copy in
  place, and every whitebox row waits for that row. A file that only *spawns*
  a child needing the unsealed engine is not of that kind: it names one itself
  through `test/whitebox-child.f` (`PROVIDE`, `ENGINE$`, `ENV!`) and stays a
  plain `SUITE`, green on its own; loading that file is what makes the row
  wait for the build row.
- The gate settles five keyed images, each once per run in a pool row of its
  own started beside the first rows (`test/gate-images.f`): the fixture writer
  and the cold host it emits (`test/fixture-writer.f`, `test/cold-engine.f`;
  rows `fixture-writer-build`, `cold-engine-build`), the saver and linker
  images (`test/app-image-engine.f`, `test/preloaded-engine.f`;
  `app-image-build`, `linker-build`) and the unsealed engine
  (`test/whitebox-engine.f`; `whitebox-engine-build`). A row needs an image
  when its load closure holds the image's module: the files it loads and what
  those import, or launch as a `.f` source through a path literal a known load
  helper consumes (`test/load-refs.f`, the reader the entry guard uses).
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
  image with `1 set-tier`, link only subjects that reach nothing the linker's
  load holds, and keep a row whose children run `ENGINE-CANDIDATE:PATH$` off
  the images. That load ran before the capture window opens, so its modules,
  names and cells are not the ones a production build carries: a maker on the
  linker image refuses a closure reaching one of its words or cells by name,
  and a subject defining one of its words dies at that line, as it dies on
  the engine at the library's line (rule 3).
  A row builds a subject that needs a module of the linker's lib closure on
  the engine (`tools/hb-build-test-lib.f` `HBT-KEYED!`). A row whose claim is
  the saver's or the linker's own load (`test/app-image.f`) keeps compiling
  them from source.
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
  the code is 1 to 255; [debugging.md](debugging.md) has the exit rule. The
  pool labels a row `TIMEOUT-UNDER-LOAD` when it exits 67 with its own report
  of `E-PROC-TIMEOUT` (-2502) as the last stderr line (`test/gate-pool.f`
  `GT-POOL-INNER-TIMEOUT?`).
- A deadline in a build child reaches the pool as a timeout through exit
  statuses, because a throw code cannot cross a process boundary.
  `tools/native-build.f` and `tools/build-fixpoint.f` exit 124
  (`PROC-TIMEOUT-RC`, `lib/process.f`) when a deadline expired and 74 for any
  other failure they catch, after naming its throw code. build-fixpoint's
  `BF-RC0`, the build-fixpoint rows (`tools/build-fixpoint-test-lib.f`
  `BFT-FIXPOINT-RC`) and `test/whitebox-engine.f` throw `E-PROC-TIMEOUT` again
  on 124, and the rows rethrow it once the step is named.
- A test's own capture deadline is no assertion result. `PROC-OUTCOME>RC`
  (`lib/process.f`), `GT-RC@` (`lib/test/runner.f`), `GE-FAIL`
  (`test/gate-common-lib.f`) and the `lib/test/outcome.f` asserts that want an
  exit or a signal throw `E-PROC-TIMEOUT` for an expired deadline, and a test
  that MATCHes the outcome itself throws it from its `timeout` arm, so the row
  reaches the pool as `TIMEOUT-UNDER-LOAD` instead of failing an assertion on
  137 or on a false exited flag. A deliberate kill is a `signaled` outcome and
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
