# Native test suite

Run focused tests through their real load paths. Run the full native registry
only when a change affects shared checker or compiler behavior, runtime ABI,
capture format or several libraries, or when focused tests cannot bound its
effects. Where the change lives does not decide this: an engine change whose
effects focused tests bound needs only those tests.

## Focused tests

Run a test file directly with `bin/hb --load`, naming on one command line the
library files the engine does not bake ahead of it, as the test's header
states; never concatenate files with shell logic.

The runner prepends nothing to a registry row, so a focused run of a row's
files measures what the row measures.

Run chosen tests of the suite in its parallel pool by name; an unknown name is
refused with exit 64:

```sh
bin/hb --load test/run.f -- lsp-boundary websocket
```

A lane's proof is its engine build and the tests of a few seconds that load the
files it changes, picked from that code, never copied from another lane's
list. Tests that run for minutes, and the full test suite, run once on the
landing tip. The full suite holds every test, so a tip that runs it runs no
test separately. A list of 21 tests run one at a time takes 8.3 minutes, six of
them 429 s, and the full suite runs all 21 again.

## Qualifying a candidate

Tests of behavior baked into the engine run on a product candidate built from
a private copy of the matching tree with a ready native engine as host:

```sh
TMPDIR=$TMP $HOST --load tools/hb-build.f -- --engine -o $OUT
```

An engine build takes at most 5 minutes; a build that runs out of its limit has
a defect to root-cause, never a longer limit.

Copy that candidate and its matching `hb.map` into the private tree's `bin/`
as regular files, not symlinks, and run the tests from that tree: the focused
tests, and the registry when the rule above calls for it. Keep its `TMPDIR`
outside the checkout: an object's path repeats its source's, a file walk skips
`.out` (src/lib/fs.f `SKIP-DIR?`), and under a `TMPDIR` inside `.out`
test/file-objects.f finds no object of the sources it writes there. Tools
that name an image's words read the `.map` beside it (tools/image-names.f),
so a stale map names the wrong words.

```sh
bin/hb --load test/run.f </dev/null
```

The convergence check's fixpoint is its last generation, never the host it
was given; after a recovery, gate the converged hb, not the trusted one.

Multi-generation convergence is a separate check for codegen, self-hosting or
capture changes that can alter successive engine output; see
[bootstrap.md](bootstrap.md#convergence-check).

### A lane rebased over code its change reaches

A rebase merges a line master added after the lane's base without a conflict
even when that line uses a name the lane renamed, or a form a new checker rule
refuses, so the rebased tree fails at that line while every row the lane ran
stays green. Before landing a lane that renames or repackages words, grep the
whole tree for every old name it reports. Before landing one that renames words
or changes what the checker admits, run every row that loads a file master
added or changed since the lane's base. The FS package lane rebased over three
renamed names, in src/lib/test/fifo.f, src/lib/net/ws-test.f and
src/lib/zip.f, none of which its own rows loaded. The cast-owner lane, gated
before src/lib/net/model.f existed, refused that file's cast of arena bytes
into a record holding another package's OPAQUE cursor, and row model-test went
red on master.

## Running the registry

`test/gate-stdlib-cases.f` is the registry. Each `SUITE` row runs its listed
files through the tree's `bin/hb`. Host requirements are in
[bootstrap.md](bootstrap.md#requirements). On macOS every test that signs
(`src/lib/codesign-test.f`, any build or snapshot write) needs team 8TW82D98K7's
signing identity in the keychain, and without it stops with
`CODESIGN:E-IDENTITY`.

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

The pool keeps no clock: sleep and priority slow a row, and only a deadline
the suite sets itself can end it.

### ARM-only rows

The registry includes `test/gate-arm-cases.f` only on an ARM host; its rows
need the ARM host, its source recovery or the JIT, and its comments state each
row's reason. Mixed rows stay registered on x86-64, where their tier-0 and
tier-1 cases must run; a fixture may guard an individual ARM-only premise after
measuring the native refusal.

### Reading a red run

Failures print the suite label, exit outcome, and captured stdout and stderr.
A row ends by its exit or by a signal, and its outcome line names which.

Every `GT-HEARTBEAT-MS` (`src/lib/test/runner.f`) the gate prints a line for
each live row: `WAIT: <label> (<elapsed> ms, cpu <total> ms, +<delta> ms)`,
the CPU time of the row's whole tree. A row whose delta stays at zero is
blocked; one whose delta never stops growing is spinning. A negative delta is
a process that left the row's tree. End such a gate with SIGINT, or with
SIGTERM when a non-interactive shell started it in the background: that shell
starts it with SIGINT ignored, and the gate keeps an inherited ignore
(`test/gate-signal-test.f` "AN IGNORED SIGNAL"). Before it kills its rows it
prints each live row's WAIT line and the tail of its output.

A red that appears only when the box is loaded is the box: reproduce it on the
unmodified base under the same load, then rerun the suite alone. Run against a
matching private source and engine tree when the result must qualify that
exact pair.

### Scratch and cache

The run removes its temporary root whether it is green or red, so the printed
tail is what a finished red run leaves. To keep a run's trees, give it its own
`TMPDIR`, the one scratch root: every maker goes under it, and a missing or
non-directory `TMPDIR` is refused by name (`FS:MKTEMP-DIR`, src/lib/fs-mutate.f).
Every gate run gets its own `TMPDIR` root. Keep it short: nothing refuses a
long one up front (`GT-ROOM-CHECK` checks
only the root's own room), the deepest row path under it must still fit
Habu's path capacity, and a row that binds a Unix socket under it has 103
bytes of socket path on macOS (107 on Linux), of which macOS's default
`/var/folders` `TMPDIR` already spends 97: the fs-mutate socket row fails
there when run on its own.

Build outputs live with the tree, under `<cwd>/.out/` or `$HABU_OUT/`
(src/lib/build-out.f states the layout). They are
disposable: no other cache exists, deleting them costs only a rebuild, and a
deliberate cold measurement selects a fresh `HABU_OUT`.

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
  entry and any import or launch of one (`test/gate-entry-guard.f`, over
  `test/load-graph.f`); `bin/hb --load test/gate-entry-guard-test.f` checks
  this without starting the full suite.
- Measure a row's CPU outside the gate, user plus system from
  `/usr/bin/time bin/hb --load <file>`. Reduce repeated work and share setup
  before splitting a slow row.
- A suite whose assertions depend on the compiler tier selects it itself, with
  `1 set-tier` after the harness and tool requires and before the code under
  test, so the harness is not compiled at tier 1.
- `bin/hb` with no argument reads its program from stdin: a pipe left open
  blocks it before anything runs and a terminal gets the REPL — it looks like a
  hang, a WAIT line with no CPU delta. Any argument names what runs, and stdin
  is then only the program's data.
- A CLI tool that reads the ambient argv is never `include`d into a shared
  image: it would read the harness's argv. Run it spawned, and keep its
  assertion in an argv-free `-test.f`.
- `SUBJECT:RUN` forks the live test process: call it with no package open, and
  never gate a CLI file that parses argv.
- A row that hangs is named by its WAIT line and ended by hand, never by a
  clock. A test that sets a deadline of its own and expects it asserts it; a
  failing row is never fixed by raising a deadline, retry count or sleep.
  [debugging.md](debugging.md) has the exit rule.
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
- No test creates a tree or path Finder cannot delete, and each proves its
  behaviour with a small fixture.
- A before-and-after timing runs on an idle host (spark, Linux arm64), both
  sides on the same machine and engine, never on the shared Mac.

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
- A performance claim compares instructions retired from baseline and
  candidate runs interleaved in one session, several rounds each. Separate
  sessions drift: zz's run phase moved 40M instructions (0.3%) between two
  sessions hours apart with unchanged code, more than many single changes.
- Tests and measurements establish different properties. Describe what was
  actually checked without mandatory mutation campaigns or extra ledgers.
- "Fails before" a tool change needs the old tool, not an old engine: an
  engine runs the tree's source-loaded tools (tools/check.f and what it
  loads), so a swapped engine reruns the new tool and shows the change's own
  failures on both sides. Revert the tool source in a scratch copy of the
  tree and run it through an entry file at the copy's root: `--load` of a
  file below the root resolves `require tools/…` against the running hb's
  tree (card section 7), so loading a test below the copy's root directly
  runs the workspace's tool.
- Behaviour is a defect only against its module's contract: read the
  header before reporting one. SECRET on Linux reads only, and its WRITE and
  ERASE throw E-UNAVAILABLE unless the host binds its own store with
  SECRET:STORE! (src/lib/secret.f); that is its contract, not a defect.
