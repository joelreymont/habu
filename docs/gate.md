# Native Test Suite Architecture

The native test suite proves one thing: the current source can build a correct
candidate `bin/hb`, and that candidate passes the checker, engine, CLI, stdlib,
PTX, and builder contracts without hiding failures behind generated runner
state.

The test suite is too slow when it treats every proof as a process boundary. A
snapshot that only launches child `hb` processes is still a launcher. The
durable shape is `bin/hb --load test/run.f`: a resident checked test-suite
controller with explicit proof subjects and small process-boundary sentinels.

## Proof Subjects

Every test in the suite belongs to exactly one subject.

| Subject | Runner | Purpose |
|---|---|---|
| `host-source` | resident suite image | Source lints, semantic tool cores, JSON/schema checks, docs checks, and other host-only assertions. |
| `candidate-cli` | exact `HABU_UNDER_TEST` executable | Public CLI behavior: startup, `--load`, stdin, argv/env/cwd, stdout/stderr, exit code, timeout, PTY/REPL. |
| `candidate-source` | exact `HABU_UNDER_TEST` executable, batched | Source programs whose semantics depend on the built candidate, not on the resident host engine. |
| `artifact` | candidate build path plus host assertions | AOT, snapshots, image bytes, signatures, and generated build products. |

The resident runner may prove host-source facts in-process. It must not prove a
candidate fact by accident. Candidate tests print and record the candidate path
and SHA before the test runs.

Candidate production is not candidate validation. The build test only produces
`HABU_UNDER_TEST`; a separate candidate validation test always runs after that
capability is ready, whether the candidate came from a build or explicit
`--under PATH`.

The explicit reuse path is:

```sh
bin/hb --load test/run.f -- --under bin/hb
```

`--under` copies the executable into the suite-owned temp root and marks the
candidate capability ready. The candidate build phase is not scheduled.

A whole-gate scheduling regression cannot run inside `test/run.f` or a slice
that it schedules: its child would enter the same gate recursively. Run these
two candidate-scheduling checks separately before landing gate-runner changes:

```sh
bin/hb --load test/candidate-rebuild-test.f -- rebuild
bin/hb --load test/candidate-rebuild-test.f -- import
```

The first requires two ordinary runs to build phase 15. The second requires an
ordinary source build followed by exact `--under` import with no phase 15.

## Boundary Rule

Semantic checks run in-process by default. A child process is justified only
when the contract is the process boundary itself or when the current compiler
path still exits through `die`/native exit instead of a catchable result.

Tool tests therefore split into:

- core semantics: checked words called in-process;
- CLI sentinels: one small wrapper test per argv/stdin/env/stdout/stderr/exit
  contract;
- examples: real `hb --load ... -- args` runs against `HABU_UNDER_TEST`;
- unchecked/catchability gaps: dotted until the compiler/evaluator exposes a
  catchable API.

Dictionary/checker negative tests follow the same split. Pure checker negatives
use direct checker/all-errors APIs. Runtime compiler errors stay as process
sentinels until the evaluator can return a result object instead of terminating
the process.

## Test Groups

The suite reports named test groups, not anonymous scheduler tests. A group name
is the report header and includes its execution policy:

```text
PASS: GROUP: stdlib/tool-repair [parallel]  (8106ms)
```

Parallel groups enter the shared process pool. Sequential groups drain the pool
before and after their group run. A sequential group is for isolation barriers
and ordered aggregate proofs only; it must not be used to hide missing setup
factoring.

Suite setup owns shared binaries and build artifacts. Tests do not list shared
setup loads: ordinary setup builds the candidate; only maker/build artifacts
are content-key reused. Groups choose a mode, and tests load only their test
entry files or execute in-process words from the resident runner. Successful
setup is silent. Setup failure prints the failing setup path as a failure; it
is not reported as a passing test.

## Metrics

Counts are not enough. The stats log records event counters and timed tests:

```text
inner-hb-spawn
span<TAB>33634<TAB>native stdlib tool-boundary slice
span-load<TAB>412<TAB>test/run-worker-stdlib.f
```

Spans have two kinds. `span` is test-time work: a test file run or a phase
total. `span-load` is load/setup time: worker support `require` lists and the
shared stdlib setup. Slowest-test attribution reads only `span` rows, and
duplicated `span-load` labels show which load lists are still repeated across
workers.

Every span label has exactly one owning emitter. The pool pass-hook owns the
span for each pool entry (spawned or forked); a fork child records its fork
label at fork entry and `GS-SPAN` suppresses a child emission whose label
equals it, so fork-backed tests are not double-counted.

Process telemetry is emitted at the four checked exec chokepoints and the
checked fork chokepoint, not at wrapper call sites. A successful exec emits
exactly one row; a failed exec emits none. A successful fork emits exactly one
parent row; child and failed-fork returns emit none. Reaper work is classified
separately:

```text
process-exec<TAB>generation<TAB>owner<TAB>candidate|baseline|other<TAB>path
process-fork<TAB>generation<TAB>owner<TAB>-<TAB>direct|reaper
```

The explicit owner is the authoritative pool slot or helper label; otherwise
the active child label or executable path is used. Performance ratchets are
owner-local: unrelated concurrent phases and process-contract fixtures remain
visible in the total without consuming another phase's budget.
`tools/process-primitive-lint.f` makes the census closed-world by rejecting raw
spawn/fork primitive calls outside the checked process modules and the checker
primitive declaration table.

The summary must show:

- total top-level phases;
- result/maker/artifact-cache and candidate counters;
- child-Habu, process-exec, candidate-exec, process-fork, process-reaper, and
  backward-compatible helper-spawn counts;
- in-process evaluation count;
- span count and slowest span label;
- load-span count and total load milliseconds (`load-spans`, `load-ms`);
- spans with no matching test row (`span-stray`).

Each phase also emits per-test subject metadata:

```text
test<TAB>label<TAB>subject<TAB>runner<TAB>boundary<TAB>sha
```

The summary joins `span` rows to test rows by label and prints one
critical-path line per subject:

```text
subject host-source: spans=29 total-ms=118327 max-ms=12951
```

That answers which subject owns the critical path and which load lists are
still duplicated. File-level test spans without a phase test row stay
drill-down detail and are counted as `span-stray`.

## Failure Triage

Phase failures are fail-soft. Every pool child streams stdout/stderr to
per-entry `pool-<gen>-<seq>-out.log` / `pool-<gen>-<seq>-err.log` files under
the suite temp root while the pool keeps a bounded in-memory tail for dumps.
When a phase fails or times out, the pool prints the tail (prefixed by a
`[tail truncated N bytes; full capture: <path>]` marker when the tail
overflowed), the outcome kind/code, the `stdout-file:`/`stderr-file:` capture
paths, and a `FAIL:` line, records the phase in the red list, and keeps
draining scheduled work instead of killing siblings.

Draining stays fail-closed. A nested suite's `GT-POOL-DRAIN` dies after the
drain when reds exist, printing a `red phases: N` summary with one
`RED: <label> kind=K code=C out=<path> err=<path>` line per failure. The top
runner prints the same red list at completion, keeps the suite temp root
(`capture root kept: <GT-ROOT>`) so the capture files survive for triage, and
exits nonzero.

`kind` is `exit` (child exited nonzero), `signal` (child died on a signal the
pool did not send — an external SIGKILL such as OOM/AMFI, reaped as `signaled`),
or `TIMEOUT-UNDER-LOAD` (the pool's OWN per-slot timeout/reaper SIGKILLed the
slot; `GT-POOL-TIMEOUT` set the `timed-out` flag before the kill). A timeout is
still a red/failing phase — it never weakens the gate — but is attributably
distinct so a contended-host kill is not misread as a genuine failure. A
`TIMEOUT-UNDER-LOAD` line carries a saturation suffix
`sat=<live>/<limit> waits=<n> ran=<ms>ms` (live slots at the kill over the pool
limit, WAIT-heartbeat count, and how long the slot ran), and the pool also
writes a distinct `pool-timeout` row to `gate-stats.tsv` for machine RCA.

Infrastructure failures are distinct from test failures: pool spawn, poll, or
capture-open errors are a fast abort that kills all live children and throws
instead of recording a red phase.

## Host Profiles

Timing regression checks are host-specific. Use named profiles instead of
remembering slot counts or cache state:

| Profile | Host proof | Slots | Nested |
|---|---|---:|---:|
| `macos-arm64-10x2` | macOS ARM64 target | 10 | 2 |
| `jetson-orin-clocks-4x2` | Linux target, NVIDIA Jetson model, CPUs `0-7` online | 4 | 2 |
| `linux-arm64-4x2` | Linux ARM64 target | 4 | 2 |

The default profile is `auto`: the runner inspects the target and host files
before the suite starts. Manual `--perf-profile NAME` forces a profile. Pool
overrides must appear after the profile; top-level `--pool-slots` is capped at
12.

`--cold-cache` selects a private per-run scratch cache root under the suite temp
directory, disables the result cache, and measures builder, maker, and artifact
cache fill without deleting the default persistent caches.

Wrap the command with `/usr/bin/time -p` when comparing end-to-end shell wall
time across hosts. `test/run.f` runs the suite directly in `bin/hb`; no
top-level test-suite snapshot is built. The side-effect-free implementation
lives in `test/run-lib.f`; invoking it directly is for focused harness debugging
only. Current commands live in `skills/habu-host-profiles/SKILL.md`.

## Implementation Sequence

1. Test timing metrics and slowest-test summary.
   Acceptance: a full test suite prints `spans=N` and `slowest-test=...`.
   Status: implemented in `test/gate-stats.f`; pool passes emit span records
   through a checked `defer` hook.

2. Candidate production and validation split.
   Acceptance: `--under PATH` prints `candidate=0`, `candidate-import=1`, and
   `candidate-validate=1`; an ordinary invocation prints `candidate=1` and
   `candidate-validate=1`.
   Status: implemented in `test/run.f` and `test/gate-engine-lib.f`.

3. Replace index-only phase dispatch with a checked manifest.
   Acceptance: every current `test/run.f` phase has manifest metadata with label,
   subject, runner kind, deps, timeout, and pool policy.
   Status: test telemetry is implemented and schema-tested; dispatch is still
   index-backed and should become table data.

4. Split mixed tool tests.
   Acceptance: semantic tool tests run as independent resident-runner tests; CLI
   tests remain explicit boundary tests.
   Status: implemented for repair, doc/schema, split lint, typed-local,
   diagnostic SARIF/public-signature and JIT dump semantics.
   Current runs distinguish ordinary candidate production
   (`candidate=1`) from explicit import (`candidate-import=1`, `candidate=0`).
   Maker, artifact, and result caches remain independent
   of candidate production.

5. Inline host-source semantic suites into the resident runner.
   Acceptance: `tool-boundary`, `lint-tools`, doc/schema, and typed-local
   semantic tests no longer spawn a child just to reload the same support files.
   Status: test-level resident execution is in place, including stdlib tail
   semantic slices, `lint-artifacts/fast`, and the `lint-libs`
   core/PTX/PTX-negative/PTX-toolchain groups. The parent loads the common stdlib
   tool base once as explicit test-suite setup, then forks stdlib workers that
   inherit it copy-on-write. Remaining work is inside true CLI/process tests:
   remove helper children only when a checked result API can preserve the same
   public boundary proof.

6. Add direct checker/all-errors source APIs.
   Acceptance: dictionary/checker pure negatives do not spawn candidate `hb`;
   process tests remain only for public fail-closed CLI behavior.
   Status: diagnostic JSON/all-errors checks and the engine source-list support
   check use the direct checker core in-process. Structure field misuse now uses
   a transactional resident `CHECK-CANDIDATE!` assertion instead of launching
   `tools/check.f`. One dictionary CLI sentinel remains for public fail-closed
   stderr behavior.

7. Add a transactional evaluator/compiler result API.
   Acceptance: source-language misuse tests can assert rc/stdout/stderr
   in-process without corrupting dictionary, checker, package, fd, or data-stack
   state.

8. Batch candidate-source probes.
   Acceptance: candidate exactness is preserved while related source programs
   share one `HABU_UNDER_TEST` launch.

9. Remove fixed drains in favor of dependency-ready work.
   Acceptance: no ready phase waits behind an unrelated child
   suite; only explicit deps and isolation barriers drain the pool.
   Status: top runner snapshots and fixed runner slots are removed. Remaining
   drains are isolation barriers or candidate readiness dependencies.

## Targets

Short-term Jetson/Orin target: warm builder, maker, artifact, and result caches,
uncontended full gate passes.

Architecture target:

- `inner-hb + inner-hb-stdin <= 15`;
- candidate validation uses at most 3 worker execs plus 8 nested execs;
- normal runtime uses at most 2 owner execs plus 10 subject execs and 10 seconds;
- the stdlib process tail retains exactly 15 direct execs and 167 isolated
  subject cases, with a nominal 10-second group ratchet;
- `boundary <= 20`;
- slowest host-source semantic test under 10 seconds on Jetson/Orin;
- hot Jetson/Orin median near 30 seconds once candidate-source batching and
  dependency scheduling land.

Global exec/fork totals are diagnostic census values, not performance limits:
process API tests intentionally launch large fixed matrices, and co-located
reapers depend on whether a case runs inside a pool worker. Tail child timeouts
use bounded `HB_LOAD_PCT`, including the structural pool-pressure floor;
performance ratchets use measured `HB_CAL_PCT` only, preserving the nominal
8/10-second limits on an idle full gate. Per-owner ratchets catch semantic
engine-boot regressions without conflating those contracts.

Generated stats, caches, build images, and test logs remain local artifacts and
are never committed. Standalone snapshot-launcher tooling is not part of the
native suite harness.
