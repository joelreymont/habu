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

`test/gate-stdlib-cases.f` is the registry. Each `SUITE` row runs its listed
files through the tree's `bin/hb`; there is no second test inventory.

Failures print the suite label, exit outcome, and captured stdout and stderr.
On failure, the full capture files remain under the printed temporary root;
successful runs remove it.

## How a suite runs, and what that demands of its files

- A `SUITE` block is **one** `bin/hb --load` spawn: its files load into one
  image in order. A suite file is therefore package-scoped, duplicate-safe, and
  closes its `package` before `T-REPORT`; a later suite otherwise dies exit 75
  with a bare token, one or two suites after the culprit. Fixture identities
  carry the test's own tag so two suites never intern one name.
- `bin/hb file.f` (no `--load`) drops to a REPL after a clean load and blocks
  on stdin — it looks like a hang, rc 124 under a timeout. Pipe `< /dev/null`,
  and give a spawned build child `/dev/null` stdin rather than letting it
  inherit one.
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
