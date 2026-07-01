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
capability is ready, whether the candidate came from a build, the content cache,
or explicit `--under PATH`.

The explicit reuse path is:

```sh
bin/hb --load test/run.f -- --under bin/hb
```

`--under` copies the executable into the suite-owned temp root and marks the
candidate capability ready. It does not install into the content cache.

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

Suite setup owns shared binaries and content-keyed build artifacts. Tests do not
list shared setup loads: setup builds or reuses the needed candidate/maker
artifact by content key, groups choose a mode, and tests load only their test
entry files or execute in-process words from the resident runner. Successful
setup is silent. Setup failure prints the failing setup path as a failure; it is
not reported as a passing test.

## Metrics

Counts are not enough. The stats log records event counters and timed tests:

```text
inner-hb-spawn
span<TAB>33634<TAB>native stdlib tool-boundary slice
```

The summary must show:

- total top-level phases;
- cache/candidate counters;
- child-Habu and helper-spawn counts;
- in-process evaluation count;
- span count and slowest span label.

Each phase also emits per-test subject metadata:

```text
test<TAB>label<TAB>subject<TAB>runner<TAB>boundary<TAB>sha
```

That lets the summary answer which subject owns the critical path and which
load lists are still duplicated.

## Host Profiles

Timing regression checks are host-specific. Use named profiles instead of
remembering slot counts or cache state:

| Profile | Host proof | Slots | Nested | Persistent budget | Scratch-cache budget |
|---|---|---:|---:|---:|---:|
| `macos-arm64-12x2` | macOS ARM64 target | 12 | 2 | 40000ms / 45000ms wall | 70000ms / 70000ms wall |
| `jetson-orin-clocks-4x2` | Linux target, NVIDIA Jetson model, CPUs `0-7` online | 4 | 2 | 100000ms / 110000ms wall | 150000ms / 160000ms wall |
| `linux-arm64-4x2` | Linux ARM64 target | 4 | 2 | 120000ms | 150000ms |

The default profile is `auto`: the runner inspects the target and host files
before the suite starts. Manual `--perf-profile NAME` forces a profile. Manual
`--pool-slots`, `--nested-pool-slots`, `--budget-ms`, or `--wall-budget-ms`
arguments override the profile when they appear after it. Top-level
`--pool-slots` is capped at 12.

`--cold-cache` selects a private per-run scratch cache root under the suite temp
directory, applies the profile scratch-cache budget unless the user supplied
explicit budget arguments, and proves content-cache fill behavior without
deleting the persistent cache.

If a default persistent-cache run discovers a missing `HABU_UNDER_TEST` after
argument parsing, the runner uses the same scratch-cache budget unless explicit
budget arguments were supplied. Use
`--cold-cache` when explicitly measuring builder/maker artifact cache-fill
behavior.

The runner's wall budget is monotonic elapsed test-suite time; wrap the
command with `/usr/bin/time -p` when comparing end-to-end shell wall time across
hosts. `test/run.f` runs the suite directly in `bin/hb`; no top-level test-suite
snapshot is built. The side-effect-free implementation lives in
`test/run-lib.f`; invoking it directly is for focused harness debugging only.
Current commands live in `skills/habu-host-profiles/SKILL.md`.

## Implementation Sequence

1. Test timing metrics and slowest-test summary.
   Acceptance: a full test suite prints `spans=N` and `slowest-test=...`.
   Status: implemented in `test/gate-stats.f`; pool passes emit span records
   through a checked `defer` hook.

2. Candidate production and validation split.
   Acceptance: persistent cache or `--under PATH` prints `candidate-build=0` and
   `candidate-validate=1`; build misses still install a stamp-backed executable
   cache entry.
   Status: implemented in `test/run.f` and `test/gate-engine-lib.f`.

3. Replace index-only phase dispatch with a checked manifest.
   Acceptance: every current `test/run.f` phase has manifest metadata with label,
   subject, runner kind, deps, timeout, and pool policy.
   Status: test telemetry is implemented and schema-tested; dispatch is still
   index-backed and should become table data.

4. Split mixed tool tests.
   Acceptance: semantic tool tests run as independent resident-runner tests; CLI
   tests remain explicit boundary tests.
   Status: implemented for repair, doc/schema, split lint, and typed-local tool tests.
   Current macOS proof after removing top warm launchers, replacing the
   monolithic parent support load with explicit suite setup, moving the
   check-tool file-label smoke to the in-process checker core, and splitting AOT
   closure diagnostics from the maker path: 22.273s internal / 24.49s shell wall
   hot, `inner-hb=1`, `inner-hb-stdin=4`, `boundary=5`, `helper-spawn=29`;
   `check-cli` is 2.557s, dictionary/checker is 5.451s, AOT negative is 2.034s,
   and the slowest test is engine fixture at 13.566s.

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

Short-term Jetson/Orin target: persistent content cache, uncontended,
`--budget-ms 70000` passes.

Architecture target:

- `inner-hb + inner-hb-stdin <= 15`;
- `helper-spawn <= 25`;
- `boundary <= 20`;
- slowest host-source semantic test under 10 seconds on Jetson/Orin;
- hot Jetson/Orin median near 30 seconds once candidate-source batching and dependency
  scheduling land.

Generated stats, caches, feature-test warm images, and test logs remain local
artifacts and are never committed. Warm images are tested by the `tail-warm`
feature slice; they are not part of the native suite harness.
