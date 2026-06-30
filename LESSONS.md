# Lessons

Last updated: 2026-07-01

Concise findings only: what worked, what failed, why. Coding standards live in
`docs/forth.md`; API details in `docs/` near their feature. One tight bullet per
lesson — keep the specific word/code/path, cut the prose.

## Checker Soundness

- **Parser-word table rows must execute at top level:** `PRIM:` originally sat
  inside `PT-*` colon words, so it would compile instead of consuming the next
  source token; literal names such as `s"` then broke compilation. Primitive
  effect declarations now run top-level: `PRIM:` consumes the name token
  immediately and the `PE-IN`/`PE-OUT` constructors build the stored effect.
- **`tools/check.f` must pre-register source-local `deftype`:** preverify parses
  signatures before the child executes source, so valid nominal declarations
  looked unknown and bad declarations died as raw prose. The tool now scans
  `deftype` first inside a checker scope, registers valid names, and emits JSON
  for invalid names.
- **Symbol interners must consume miss sentinels:** `SYM-INTERN` initially left
  the `0` id returned by a failed `SYM-FIND` under the new id, corrupting the
  first `TRUST` during stage-source loading. For `( id bool )` finders, every
  miss path must `drop` the sentinel before publishing a new record.
- **Checker metadata loops need owned indexes:** the integer-type table crash was
  `CT-FIND` using `over` to recover the loop code after pushing search strings;
  it passed a pointer as the type code and `CT-NAME$` fetched through garbage.
  Store table scan indexes in named cells or locals before pushing payload data.
- **Cell `@` must reject byte spans:** `CHECK!` accepted `ptr u8 @` because the
  primitive model treated `@` as unconstrained `ptr a -- a`. The fix belongs in
  checker primitive semantics: concrete `ptr u8` uses `c@`/`c!`; pointer-valued
  cells are modeled as `ptr ptr u8` and read through `ptr-field @`.
- **Package reopen is scope, include is composition:** reopening `package NAME`
  resumes the same private/public wordlists and duplicate set; `--load`,
  source-list, or include still owns file dependency order. Do not include a
  file merely to share the package namespace.
- **Package public tails are the API:** once a module has `package TASK`, the
  public spelling should be `TASK:KILL`/`TASK:DONE?`, not global-style
  `TASK-KILL`/`TASK-DONE?`. Keep implementation helpers private and put the
  caller-facing surface in the `public` section.
- **Test fixture state belongs in a package:** focused framework tests should use
  a private package (`package FEATURE-TEST`) with short private names instead of
  global stems such as `TST-*`; only the public framework API is qualified.
- **Self-calls use `RECURSE`:** naming the word being defined inside its own body
  can compile the wrong target and crash at runtime. Recursive helpers must use
  `RECURSE`, then keep the checker regression that exercises the recursive path.
- **Checker source expansion keeps lint ownership:** `tools/check.f` flattens
  `require`/`required` dependencies into a temp source for checker execution, but
  trust/signature/boundary/reserved-name lints must still scan the original input
  path so manifest sites stay stable.
- **Core byte helpers are not string-library setup:** `lib/ffi-abi.f` used
  `BYTE-COPY`, so `include lib/ffi.f` failed unless `lib/string.f` happened to
  be loaded first. Small primitive helpers used across unrelated libraries
  belong in a narrow `src/core/*.f` prelude file, and test entry files should
  include their own setup instead of making suites encode broad library order.
- **Runtime data slots must not overlap evaluate frames:** `EVAL-FRAME` spans
  `$3800..$39FF`; putting task or FFI scratch cells inside that range corrupts
  nested `include`/`evaluate` and makes definers fail as if tasks were live.
  Keep per-runtime scratch after the full frame area and before `DATA-START`.
- **Module entry files own dependency setup:** `include lib/task.f` must work
  without the caller knowing `errors`/`memory`/`ffi` order. Test/tool entries
  use `require` for setup, and suite declarations list only entry files plus
  script args. Shared warm setup must also use `required`, and snapshots must
  preserve the require registry so baked support is not reloaded at runtime.
- **Tool entries must not borrow suite prelude:** `tools/dot-dep-lint.f` passed
  inside the stdlib suite but failed standalone at `CLEANUP-RESET` because the
  suite had already loaded `lib/fs-mutate.f`. Every tool entry must require the
  exact files needed by its own top-level path.
- **Tasked engines need process-wide fatal exits:** Linux `exit(93)` terminates
  only the calling pthread, so checker/die paths can leave workers alive and
  make process captures time out. Native and bootstrap emitters must use
  `NR-EXIT-GROUP` for process termination; `pthread_exit` remains the task-local
  stop boundary.
- **Task facilities need owner state above pthreads:** SwiftForth-style
  facilities make same-owner `GET` and non-owner `RELEASE` no-ops; a raw
  pthread mutex exposes deadlock/error behavior instead. Track an owner token in
  the facility record and keep pthreads as the private blocking primitive.
- **Warm-image tails must not reload prefix-owned layout:** target layout files
  are part of the engine prefix. Snapshot tail tools append image emitters
  (`elf.f`/`macho.f`) only; re-appending `src/os/*/layout.f` redefines image
  constants and breaks warm-image baking.
- **Duplicate package definitions belong at publish time:** package namespaces
  concentrate many natural names, so same-wordlist redefinition must fail in
  both `C-QUALIFY-DEF` and the certified checker signature recorder. Explicit
  `TRUST` remains the audited override/refinement path; normal definitions must
  not silently replace earlier package public/private rows.
- **Parallel implementations need namespaced public words:** `lib/ptx/ad.f` and
  `lib/ptx/ad-dag.f` both used `AD-EMIT-REV`; duplicate-definition rejection
  correctly stopped the combined PTX suite. Keep experimental and replacement
  implementations behind distinct public names or packages until one retires.
- **Local-first compiler dispatch still owns checker capture:** moving local
  references ahead of keyword dispatch means `C-LOCAL-REF` must call `LBCAP`
  after `LLOC-FIND`; otherwise runtime emits the local but the hook checks a
  different body.
- **Namespace qualification is only a non-edge colon:** `HB:COUNT` qualifies
  through a wordlist; `GE-FILES:` stays an ordinary Forth word. Keep `LFIND`,
  definition storage, xref, and docs on that same rule.
- **Forth language rules live in `docs/forth.md`:** checker/type/style guidance
  was moved out of this file so there is one standards source for stack comments,
  `TRUST`, roles, locals, checked DSLs, booleans, pointer fields, quotations,
  `catch`, and phase tokens.
- **Checker model cutovers must rebind the hook:** reloading
  `src/core/checker.f` and `src/core/render.f` without reloading
  `src/core/check-hook.f` left `HOOK` calling the old `CHECK!`; the generated
  bootstrap/fixpoint source has to refresh all three together.
- **Old seeds may lack envp capture:** the Linux seed could read script arguments
  but not `HB_TMP` through `GETENV`, so stage2/fixpoint now passes the temp root
  explicitly after `--` and keeps generated paths under the build driver.
- **`ptr-field` is cell-indexed:** Linux stage2 crashed in `ARGV$` because
  `src/os/*/env.f` used `ptr-field` for DATA byte offsets and pre-multiplied
  argv/envp indexes. Header reads are audited byte-offset boundaries; argv/envp
  entries use raw cell indexes.
- **Do not publish facts with empty trusted stubs:** adding
  `HB-TARGET-LINUX?`/`HB-TARGET-MACOS?` to `src/core/roles.f` as empty
  `TRUSTED:` words made `EM-ENTRY-ARGS` branch on stale stack data during
  stage2 emission. Target predicates live in `src/os/<target>/target.f`; only
  nominal identity casts belong in `roles.f`.
- **Typed storage bugs cluster around raw state:** the Linux tree-shaker/env work
  exposed pointer and boolean state cells that looked like plain `n` cells. The
  durable rule belongs in `docs/forth.md`; the lesson is that removing unchecked
  file scopes quickly finds these hidden contracts.
- **Try a checked factor before new trust:** `FS-BYTE-OFFSET` looked like it
  needed a primitive, but `: BYTE+ ( ptr u8 n -- ptr u8 ) + ;` certified against
  existing pointer arithmetic and let the trust row disappear.
- **Split generated unchecked spans by evidence:** `build-fixpoint` could keep
  `rt.f`, `crash.f`, `image-bytes.f`, and `regalloc.f` checked; only target
  image/sign, `habu1/prof`, and `jit/habu2` still need the raw emitter boundary.
- **Model phase tokens as checked cells:** `asm`/`img`/`snap` cannot be empty
  trusted ghosts when their defining files are checked; push a nominal cell at
  the producer and erase/preserve it at the next phase boundary.
- **Function-passing is a checked capability — don't default to unchecked:** I
  almost copied `combinators.f`'s `0 set-check` boundary for a generic comparator
  `SORT!`. Empirically the checker verifies a quotation parameter executed through
  a call chain AND a `?do`/`begin` loop (E1–E5 probes), so `lib/sort.f` `SORT!`
  ( `ptr a n [ a a -- bool ] --` ) is fully checked. `combinators.f` MAP/FOLD/EACH
  are an unchecked boundary that predates this and could likely be checked too —
  it is not a model to copy. The reusable rule is in `docs/forth.md`.
- **Deferred execution must carry its effect:** raw xt cells (`variable`/`@ execute`
  or `['] ... is`) let the stored implementation escape checker knowledge.
  `defer NAME ( effect )` plus `[: IMPL ;] is NAME` keeps assignment checked, and
  every source pre-verifier (`hb-build`, all-errors reducers) must learn new
  top-level definers instead of treating them as ordinary tokens.
- **Eval grading is not load checking:** `CHECK!` is the persistent load hook and
  must keep certified signatures for later definitions. Model-candidate grading
  uses `CHECK-CANDIDATE!`, which suppresses duplicate-name rejection only for that
  candidate and restores `USIGS`/`NORETS` afterward.
- **Runtime definers must publish checker facts:** a parent preverify pass cannot
  help the child checker registry. `create`/`variable`/`constant` must publish
  their `TRUST` effects in the native compiler when the hook is installed.
- **Parser-word payloads are part of the checked body:** `[char]`/`char` consume a
  source token before codegen, so the compiler body capture must append that token
  and the checker must model the parser word as one literal plus one skipped token.
- **Keep include as a two-leaf boundary:** source composition needs checked
  path/read/depth code plus only the mmap pointer refinement and final `evaluate`
  crossing as `TRUSTED:` leaves. Do not grow include to hide source-order bugs.
- **Malformed control is not uncheckable:** orphan closers and unterminated
  `if`/loop/quotation/`case` frames must set `OK=0`; `UNCK` is for missing
  checker model coverage, not syntax imbalance.
- **Do not port relative linked-list words by habit:** SwiftForth's
  `@REL`/`!REL`/`,REL`/link traversal words bake dictionary-relative pointer
  arithmetic into APIs. Habu should model node layout with structures, runtime
  collections with arrays/maps, and dispatch with checked `case` or execution
  vectors.
- **Early defining words need post-checker effects:** moving structures before
  `checker.f` let checker records use the structure DSL, but `CREATE ... DOES>`
  cannot publish effects until `trust` exists. Load definitions early, publish
  their checker rows from `src/core/structures-effects.f`, and make `DOES>`
  effect publication skip only when no checker hook is installed.
- **Checker-to-render features need hooks, not load-order shortcuts:** `checker.f`
  loads before `render.f`, so `{: x:? :}` records show-inferred locals in checker
  state and calls `LOCSHOWXT`; `render.f` installs the printer after its type
  renderer exists.

## Tool & Infra

- **Test framework and project policy are separate:** `lib/test.f` owns the
  reusable suite/group/test mechanics and setup/teardown hooks; Habu-specific
  warm images, filters, and process argv live in the Habu test adapter.
- **In-process eval needs state rollback:** `GE-EVAL-FORGET` must restore more
  than `cp`/`ndict`/`UEND`; current wordlist and `JSON-DIAGS` leaked across
  resident tests until they were snapshotted. Runtime-wide profiler state is
  not covered by that rollback, so profiler tests remain process sentinels.
- **Resident suites must preserve parallelism:** moving all `lint-libs` into one
  in-process runner serialized PTX and regressed time. Split resident groups by
  parallel-safe cohorts (`core`, `ptx`, `ptx-neg`, `ptx-toolchain`, artifact
  fast checks) so setup duplication and child `hb` launches drop without turning
  the suite single-file.
- **Resident forks need explicit harness context:** a forked in-process test does
  not see the `PROC-ENV+` vector prepared for spawned children, and `GETENV`
  still reads the host env. Install Habu-side defaults for helper spawns and
  direct warm/check overrides for in-process helpers, or resident tests rebuild
  warm images and silently lose caches.
- **End-to-end time includes entry compilation:** `test/run.f` can report
  sub-30s after `TR-GATE-START!` while `/usr/bin/time` is still >40s because
  parent load compiles resident support first. The next architecture cut must
  remove top-level support compilation from the timed command, not just optimize
  scheduled phases.
- **Content-key caches must stay memory-resident during a key build:** re-reading
  `content-key.cache` for every `CK-FILE+` made hot launchers pay repeated I/O.
  Load the cache once per root and append new rows to the in-memory copy.
- **Semantic tool slices should dispatch directly:** routing `tool-boundary-trust`
  through the generic stdlib suite kept a process/test-suite boundary on the
  critical path. Direct `GSI-*` dispatch in resident forks cuts inner `hb`
  launches and makes the slowest slice visible.
- **No-binary bootstrap is its own gate path:** Gforth runs `bootstrap/cg/forth.fs`,
  so Habu typed locals such as `done:label` are literal Gforth names and break
  codegen. The bootstrap data region must also fit static checker state
  (`USIGS-BOOT`), and its native reload prelude must reset user signatures and
  hide from `SEQ` before reloading current source.
- **Do not bake one-off AOT gate support into the warm runner:** adding
  `tools/aot-call-report.f` to the runner image overflowed the checker user-signature
  snapshot even though AOT phases are scheduled cold/early. Keep AOT-only report/assert
  helpers on the cold AOT paths and reserve the runner image for phases it actually runs.
- **Direct hb-build linting is an adapter, not a runner dependency:** baking
  `aot-lint-core`/`signature-lint-core` into `hb-gate-warm` also overflowed the
  user-signature snapshot. Keep `hb-build-lib` defaulting to child CLI lints and
  install direct lint hooks only in cold AOT phases that already load the cores.
- **Gate budgets are stop-line thresholds, not comfort blankets:** raising the
  default native gate budget to 160s hid duplicated gate work after the suite had
  already been cut near 90s. Keep the default at the current green threshold and
  use `test/run.f -- --budget-ms N` as the explicit slow-host override while the
  30s architecture work removes duplicate launches/builds.
- **Cache Habu-under-test by content, not path:** the full gate's first wall was
  a fresh fixpoint build before any under-test phase could start. Hash `bin/hb`,
  the runner/build harness, and every emitted engine/repl source; a persistent
  hit can unblock under-test slices immediately, while a miss still runs the
  normal fixpoint and installs the produced candidate under that key.
- **Candidate production is not validation:** a cache hit correctly skipped the
  fixpoint producer, but also skipped `GE-ENGINE-SUITE` and hook/dictionary
  checks because `GENG-BUILD-SLICE` mixed build and proof. Keep the producer
  build-only; always run a candidate validation row after the `under`
  capability is ready, including cache and `--under PATH` imports.
- **Persistent maker caches must be keyed by emitted maker inputs:** once
  `HABU_BUILD_CACHE` points outside the per-run temp root, `hb-aot-mk` and
  `hb-build-mk` cannot be fixed filenames. Hash the build library, loaded
  helper libs, build-fixpoint source, target source, common engine source, and
  selected AOT/REPL driver, then put the mode in the key.
- **Warm snapshots must serialize used state, not reserved arenas:** hiding
  dictionary names without rewinding `cp` leaves dead code in snapshots, and
  static checker arenas (`create ... allot`) bake reserved capacity into every
  image. Warm images now compact tail code and heap-allocate checker signatures,
  copying only used signature bytes into the snapshot payload.
- **Content keys cannot nest the global SHA context:** `SHA256-FILE` resets the
  global SHA state, so using it while an outer cache key is active collapses the
  key. Build a tagged manifest of version strings, file names, and per-file
  digests, hash the manifest once with `lib/content-key.f`, and prove
  invalidation by changing a baked source and requiring a miss.
- **SHA output ownership is caller-visible state:** `SHA256-FILE` writes the raw
  digest to `SHA-DIGEST` and resets `SHA-OUT`; wrappers such as
  `SHA256-FILE-HEX` must keep their caller output pointer separately and render
  from `SHA-DIGEST`, not from the global `SHA-OUT`.
- **Stdlib files have four registry points:** adding a library file requires the
  source file, `lib/std.manifest`, `FILEMAP.md`, and `tools/filemap-lint.f`.
  Miss one and the direct manifest gate is the first correct failure.
- **Pool slots are host policy, not universal truth:** on this macOS/aarch64
  12-core host, hot-cache full gates run 24.341s internal / 26.72s wall with
  8 top-level slots and 2 nested slots. Keep Linux conservative until measured
  there, cap dynamic slots below fixed warm/AOT artifact slots, and use
  discoverable argv knobs:
  `test/run.f -- --pool-slots N --nested-pool-slots M`.
- **`Habu-under-test` is the small engine, not a snapshot:** the native engine
  gate accidentally promoted `hb-new`, whose snapshot trailer baked `$14523b4`
  bytes of live DATA and produced 22 MB candidates that could jump into zeroed
  dictionary code on Linux. Promote `hb-stdin`, enforce a small candidate size,
  and leave snapshots as warm-runner/cache artifacts only.
- **Pipe-scoped env vars are easy to lie with:** `env HB_TMP=/tmp/x printf '' |
  bin/hb ...` sets `HB_TMP` only for `printf`; to preserve a gate temp root, put
  the env on the Habu side: `printf '' | env HB_TMP=/tmp/x bin/hb ...`.
- **Gate budget proofs need an uncontended Habu host:** full-gate timing is
  meaningless while another worktree is running `test/run.f`; a concurrent
  `habu-maki` gate pushed local runs from ~125s to ~154s and left active `hb`
  children competing for CPU. Check for active Habu gates before claiming a
  budget regression or pass. In this exec environment, focused `--load` smoke
  commands also need explicit stdin EOF (`printf '' | bin/hb --load ...`) or the
  REPL can wait on an open pipe until the harness kills it.
- **Gate phase order must follow measured tails:** adding dictionary/checker
  coverage can turn a formerly short late slice into the critical tail. Keep
  `test/run.f` `TR-LATE-ORDER` sorted by measured post-warm duration so coverage
  additions do not push a 20s+ slice behind every lint cleanup phase. Split
  check-warm-only tails (`engine fixture`, `dictionary/checker`) from
  tools-warm-dependent tails; waiting for both warm images before starting all
  late phases wastes several seconds. Record warm completion in persistent flags,
  not fixed pool-slot probes, before reusing slots for later phases. The gate
  pool poll timeout is part of the budget too; a 1000ms poll can lose the whole
  margin at the final phase, while `$64`ms keeps attribution responsive without
  busy-spinning. On this 4-online-core Linux/aarch64 target, six top-level slots
  outperformed eight because several phases spawn nested pools.
- **Do not spawn assertion tools for semantic checks:** gate JSON assertions are
  checked library words in `tools/gate-json-assert-core.f`; calling them
  in-process preserved coverage and cut hot helper spawns from 151 to 123.
- **Do not run negative compiler probes through raw `evaluate`:** positive
  `GE-EVAL-CAPTURE` can run in-process, but rejected source can exit through
  checker/compiler `die` before `catch` returns. Migrate semantic negative tests
  through `CHECK-CANDIDATE!` plus diagnostic rendering; keep true CLI/die
  boundaries as child-process sentinels.
- **Use checker-core buffer adapters for semantic negatives:** `CHECK-ALL-ERRORS-BUF`
  reuses `VERIFY-SOURCE-BUF` candidate scope and diagnostic-buffer rendering
  without writing a temp file or spawning `hb`. It is valid for ordinary semantic
  rejects such as package no-return flow and duplicate-definition. Full-source
  failures such as duplicates must be handled before per-definition isolation,
  or the fallback can falsely pass. Do not add `TRUST` for checker internals just
  to name the duplicate; keep a generic diagnostic until a typed checker
  diagnostic API exists.
- **Warm image cache publishes must be stamp-last:** a failed warm-tools bake
  wrote a new `hb-tools-warm.trust.f` beside an old image/stamp, so the next
  cache hit paired mismatched artifacts and failed with `checker: duplicate
  definition: repl-file-cap`. Build into candidate image/trust paths, delete
  the old stamp before publish, rename artifacts, then write the stamp last.
- **Warm images must clear include state before snapshot:** a support file that
  used `include` left `INCLUDE-BUFS-A` pointing at an mmap address from the
  baker process. Restored warm images then failed nested includes with
  `include: read failed`. Snapshot prep must close/reset include buffers,
  cursors, depth, and path state before `SNAPGO`.
- **Fixed artifact slots must not be reused by the dynamic pool:** warm/check/AOT
  artifact phases used fixed slots that the normal pool later reused before the
  readiness flags were marked. That made `TR-DRAIN-UNTIL-WARM` wait on unrelated
  engine/debug work and hid cold-cache overlap. Put fixed artifacts in pool
  slots outside the dynamic slot range and let normal phases use slots `0..N-1`.
- **Focused gate wins must survive the full DAG:** splitting
  `tool-boundary-lints` into parallel suites cut the focused tool slice from
  ~22.8s to ~18.3s, but hot full gate regressed slightly under contention and
  added top-level subprocesses. Keep the in-process semantic cuts; revert splits
  that only win in isolation.
- **Do not split resident groups past setup amortization:** splitting combined
  tool-doc/tool-repair residents into five single-purpose resident forks made a
  hot macOS run regress from 29.04s/26.760s to 30.40s/28.031s. Parallelism that
  duplicates resident setup and adds fork scheduling loses even when individual
  subtests look independent.
- **Inline warm-runner work must overlap child suites:** the winning
  `tool-boundary-lints` cut split tests into load-only libs, skipped the cold
  child suite, and ran inline lints immediately before `GT-POOL-DRAIN`; running
  them before spawning sibling suites serialized the slice and lost the full-DAG
  win.
- **Do not make inline gate work a serial dumping ground:** inlining
  check-repair/doc/typed-local semantics into the single stdlib hook made zed's
  tool slice wait on one resident thread for ~56s. Split independent semantics
  into first-class resident-runner rows; the hot local gate fell to 24.579s with
  `warm-miss=0` while still running candidate validation.
- **Run the suite from `bin/hb`, not a top snapshot:** replacing a broad
  top-level `hb-test-suite` image with direct `bin/hb --load test/run.f` kept
  the small engine as the entry point, removed a generated setup artifact, and
  still held macOS hot wall time under 30s. Cold/cache-fill remains a separate
  profile because warm/AOT artifacts are legitimate misses.
- **Bake repeated semantic setup into the resident runner:** the stdlib tool
  groups were still paying the same checked lint/check-all-errors setup in every
  resident process. Baking that common tool base once into `hb-gate-warm` and
  marking setup sentinels cut macOS hot wall time from 43.932s internal to
  34.837s without removing CLI boundary coverage.
- **Discovered cache misses must switch the timing profile to cold:** a default
  run after changing `test/run.f` rebuilt both warm runners but still used the
  hot 55s/60s macOS budget, failing at 62s despite all tests passing. Warm-runner
  and candidate cache misses now mark `cache=cold` and apply the profile cold
  budget unless the user supplied explicit budget args.
- **Stats schemas need content assertions, not just counters:** `GS-ROW` first
  emitted `sha/boundary/runner/subject/label` despite docs promising
  `label/subject/runner/boundary/sha`. Tests must assert representative TSV row
  content so telemetry remains usable for scheduling RCA.
- **Late-bound gate hooks need `defer`:** redefining `SUITE-INLINE-WORK` after
  `GATE-STDLIB-MAIN` compiled still called the old xt. Use a checked `defer`
  hook installed by the warm-runner entry, and execute inline work at the
  existing pre-drain point so it overlaps sibling child suites.
- **Gate caches must be default-on and content-keyed:** a 40s budget failed a
  cache-fill run at 71.015s even though the hot path was ~31s. Keep warm images,
  `hb-under-test`, maker, artifact, and file-digest caches under the default
  user cache root, remove opt-in cache overrides, and let the 70s gate catch
  real regressions.
- **Artifact caches must key content, not temp paths:** `hb-build` output caching
  first missed every run because `CK-FILE+` included the temporary source path in
  addition to the digest. Use a stable logical label plus the source digest for
  generated/temp-owned inputs. AOT-positive fell to ~13s hot, but the full gate
  barely moved because AOT-negative became the critical path.
- **Schedule early only when the artifact invariant is already true:** hot gates
  restore content-keyed `HABU_UNDER_TEST` before `TR-EARLY-START`, so source-only
  lint slices can run early under the candidate. Guard early manifest/libs on
  `TR-UNDER-READY`; leave artifact lint late because filling the early pool
  regressed the full DAG.
- **Strict duplicates expose generic fixture names:** once redefinition fails
  closed, tool fixtures named `OK`/`BAD`/`FOLD` and shared helpers named `STR=`
  collide with baked or sibling words. Prefix generated names by fixture/tool and
  keep shared lint helpers in a tool-owned vocabulary.
- **Core bootstrap helpers need private names:** do not move public stdlib words
  behind checker-hook trust to dodge duplicate definitions. Give bootstrap-only
  helpers a core-owned name such as `CORE-STR=` and let `lib/string.f` publish
  the public `STR=` contract so stale native `bin/hb` can self-refresh.
- **`is` needs checker-owned target kind:** runtime `DEFER-MAGIC` validation is
  not enough. `defer` must record a checker-visible target-kind entry, and
  `tools/check.f` must reject `is` on non-defer words before runtime.
- **Warm snapshot tails hide, they do not replay:** replaying baked core/target
  files used to dodge duplicate `SNAP-OUT`, but it also reset state by accident.
  Emit `HIDE-DEFS-FROM SNAP-OUT`, append `snap.f`, and test that old tail deps
  are absent.
- **Image mmap pointers are transient:** `MBUF-A` persisted through a restored
  image and crashed the next `IMG-M8`. Snapshot writers must clear mmap-backed
  image buffer pointers/cursors before building a new header.
- **Reserved parser names need a preflight lint:** `variable I` can pass the raw
  engine but make `tools/check.f --source-list` fail as silent rc 70 because `i/j`
  are compiler loop-control tokens. `tools/reserved-name-lint.f` now runs before
  the checker child; generated converters must run it after prefix stripping so
  naturalized names become `IX`/`JX`, not bare `I`/`J`.
- **Focused gate slices need a temp root:** direct-loading
  `test/gate-dictionary.f` does not run `TR-START`; pass `HB_TMP` and
  `HABU_GATE_WARM_ROOT` or use `test/run.f`, or warm images resolve to
  `/hb-check-warm` and fail with `E-FS-OPEN`.
- **Nested lint subprocesses need their own timeout caps:** fast tool probes can
  keep a tight timeout, but a fixture that spawns `trust-lint` or another
  repo-scale tool must use a separate cap sized for aggregate-gate contention.
- **Repo-scale source lints stream, not vectorize:** duplicate-definition linting
  generated stage2 source took >40s when it first built a full token vector. A
  streaming scanner over the source buffer cut the same 400KB stage2 file to ~2s;
  use raw `parse-name` semantics for definer payloads so words such as `(CMP)` are
  not mistaken for comments.
- **Dot blocker edges need a gate:** stale `.dots/*` `blocks:` IDs made work look
  blocked on deleted/completed tasks. `tools/dot-dep-lint.f` now walks `.dots/`
  directly and fails the lint slice on any blocker that is not backed by a dot
  file.
- **Ignored dot archive cannot satisfy active blockers:** `.dots/archive/` is not
  tracked, so a clean host may not have the same archived files. Dot dependency
  lint must ignore archive entries and active dots must drop completed blockers.
- **New PTX trusted primitives need rows before merge:** local `master` had
  `RELU`/`RELU-V4` TRUSTED sites without `TRUSTED.md` rows; the full native lint
  slice caught it. Add the row and a checked kernel fixture in the same change.
- **PTX elementwise ops share one codegen contract:** scalar and v4 tile binary
  ops route through typed opcode-to-mnemonic helpers; adding `-.`/`/.` means
  updating scalar/v4 TRUSTED rows plus both type-check and emitted-mnemonic tests.
- **Device proofs must fail closed:** CUDA/FFI proof tools are not allowed to
  print `NO` and exit success, reuse stale readback buffers, or drop Driver rc
  values. Device gates need rc-checked wrappers, private temp roots, sentinels,
  cleanup, and nonuniform multi-element goldens before they can support claims.
- **Gate speed RCA follows the phase wall clock:** warm images cut repeated
  inner-tool recompiles; the bigger wall cut came from a bounded checked DAG
  pool. Do not mutate `bin/hb` inside the gate: build candidates under private
  `HB_TMP`, run independent stdlib/diagnostic/engine slices concurrently, bound
  nested pools with `--nested-pool-slots`, and delay short timeout-sensitive
  lints until the heavy wave drains.
- **Full DAG timing beats isolated wins:** on Linux/aarch64, separate hb-build
  maker warming and warm-tools manifest loading passed focused probes but
  regressed the full gate under contention. Keep only schedule moves that improve
  the documented full command; record reverted timings in the dot so failed
  variants are not rediscovered.
- **Borrow SwiftForth semantics, not unsafe contracts:** SwiftForth's `PLACE`,
  `APPEND`, `ZPLACE`, and `ZAPPEND` explicitly do not check destination capacity.
  In Habu, borrowed string-buffer utilities must carry capacity and length cells
  (`BUF-APPEND*`) and throw named errors; do not import unchecked string mutation
  under familiar names.
- **Gate budgets must match measured host capacity:** on 2026-06-28 the full
  native gate was all-green at 100.985s internal time on the 4-online-core
  Linux/aarch64 target. A 90s default budget was below measured capacity for the
  current suite; keep the 90s/30s goal active, but use the documented full-gate
  command and phase timings to earn lower budgets instead of dropping coverage.
- **Gate instrumentation must survive cleanup:** appending counters under `GT-ROOT`
  and summarizing after `GT-CLEANUP` produces a false all-zero report. Emit the
  stats summary before deleting the gate temp tree, then enforce the time budget.
- **Shared Habu-under-test is a pipeline prerequisite:** build the candidate in
  the early engine-build slot, publish it atomically, print path+SHA, then release
  downstream phases onto `HABU_UNDER_TEST`. This preserves overlap and proves the
  rebuilt binary without serializing the whole gate behind fixpoint.
- **PTY fixtures should wait for events, not fixed quiet time:** `test/proc-pty.f`
  spent ~18.5s wall with <1s CPU because each interaction waited six 50ms quiet
  polls. Preserve max wait windows, but use small named poll intervals and a
  minimal quiet threshold so prompt-driven tests finish when the expected bytes
  arrive.
- **Batch diagnostic fixtures at the source:** if many checked rejects use the
  same tool load path, write one source file, run one `check-all-errors`, and
  assert each JSONL row by `word`/`repair_class`. Per-case checker spawns hide
  real coverage behind startup cost.
- **Merge positive builder proofs by invariant:** if two successful builds only
  differ by fixture payload, one strict feature bundle can own recursion, parser,
  closure, binary-shape, and output checks. Size caps should prove "no engine
  embed" for that bundle, not preserve a tiny-source threshold.
- **Do not lock shared maker caches:** AOT positive/negative can miss the same
  maker key concurrently. A mkdir lock timed out under full-gate contention and
  left a stale lock after pool teardown. Build makers in each private `HB_TMP`
  and publish the completed executable with atomic `rename`; cold races may
  duplicate work, but they cannot deadlock the cache.
- **Semantic check-tool fixtures should not spawn the wrapper:** reserve
  `tools/check.f` subprocesses for argv/env/stdin/exit/source-label contracts.
  `tools/check-test.f` kept positive verification and unterminated-string
  diagnostics as full wrapper subprocesses even though `VERIFY-SOURCE-BUF` and
  `CHECK-ALL-ERRORS-FILE` already owned those invariants.
- **Use checker-warm for checker CLI smokes:** when a test must keep a real
  `tools/check.f` argv/stdin boundary, run it through the checker-warm image
  with `check-core` baked and only `tools/check-main.f` loaded. The general
  tools-warm image should not recompile checker core for each smoke.
- **Batch source-list checks by dependency graph:** engine fixture checks for
  fs/process libraries spawned one checker-warm child per module group even
  though the unique dependency closure is one ordered source list. One combined
  `GE-CHECK-SRC-LIST` preserved certification coverage and cut the focused
  fixture slice by about 10s.
- **Nested gate captures report outcomes:** under full gate concurrency, 1s/5s
  `RUN-ARGV-CAPTURE` calls can throw silently before `T-REPORT` (`rc 58` is
  `E-PROC-TIMEOUT`). Gate boundaries use outcome capture plus attribution:
  case/phase, executable, argv/load list, outcome kind/code, named rc, capture
  bytes/cap, stdout, and stderr.
- **Gate entries catch their own throws:** a direct `test/gate-engine.f -- build`
  can fail before child attribution runs. Entry wrappers catch top-level throws,
  print the phase label plus throw code/name, then rethrow so rc stays useful.
- **Small-engine suites use test-owned checker fixtures:** `bin/hb` must not bake
  image/spawn emitter words just so checker algebra probes can run. Use local
  `T-*` TRUST rows for role algebra in `test/engine-suite.f`; keep real emitter
  source coverage in build-fixpoint/source-shape tests.
- **Manifest lint needs top-level scheduling under load:** `stdlib-manifest-test`
  was fast alone but its internal `public-signatures` child hit the old 5s
  timeout under full-gate contention and surfaced only as `rc 58` until pool
  outcome attribution printed the throw code. Schedule the manifest lint as a
  direct gate phase, size its child timeout for aggregate contention, and keep
  the test instead of hiding it inside a nested batch.
- **Outcome helpers belong in process libraries:** `gate-common` had duplicated
  the env/no-stdin outcome capture loop because `lib/process-env.f` only exposed
  the stdin outcome variant. Add missing `RUN-ARGV-ENV-*-OUTCOME` helpers in the
  process layer, with `process-env-test`, manifest, and docs rows, before wiring
  gate attribution to them.
- **Pool slot state is an invariant:** free=`-1`, active=`0`, done=`1`; set
  active before spawning. A live-count pool with free slots still marked free
  ignores its fds and spins forever after children exit.
- **Native port gates do not need language runtimes:** port validation proves
  `bin/hb`, target source selection, syscalls/env, ELF AOT, checker/lints,
  self-refresh, REPL, and Habu tooling on the target. Do not install JS/Python/
  Rust/TS/model stacks to prove a native port.
- **One public binary:** `bin/hb` (tty REPL, piped stdin, `hb script.f args`, or
  `hb --load lib.f tool.f -- args`). Build-only engines stay temp under `HB_TMP`;
  no legacy aliases/maker binaries.
- **Pipe vs script vs source-list mode:** non-tty stdin with bytes = pipeline mode
  even if `argc>1` (`hb seed count < prog.f`); empty non-tty stdin + `argc>1` runs
  `argv[1]` as a script; `--load src... -- args` reads listed files and leaves fd0
  as tool data (`READ-STDIN-ALL`). Reproduce gate check failures with raw stdin +
  the wrapper's own prefix — not file-argument mode (different code path).
- **No-binary recovery installs only `bin/hb`** (generated/ignored): recover with
  `HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh`; Gforth makes only `HB_TMP`
  artifacts, then `bin/hb` self-refreshes. Daily work never uses the Gforth path.
- **Bootstrap source parity matters:** `tools/bootstrap.sh` must append the same
  native source layers as `tools/build-fixpoint.f` (including
  `src/os/image-bytes.f`). Keep bootstrap `bootstrap/cg/forth.fs` emitter calls
  syntactically real (`MOVZ,`, `BCOND,`, `ADDI,`) because Gforth catches the
  comma-less raw word immediately.
- **Gforth host needs locals:** Homebrew 0.7.3 can't parse `{:` and isn't usable;
  use snapshot `0.7.9_20260610` (macOS: configure `UNSUITABLE_CC=none`, wrapper
  execs `gforth-itc -i gforth-light.fi`) or `GFORTH=…/gforth-fast`. Bootstrap
  probes must check output (exact `1`) AND exit 0 — 0.7.3 reports the locals error
  yet lets later shell stages continue.
- **Target source lists are a build invariant:** bootstrap and native fixpoint
  select `src/os/<target>/{sys,env,image,sign}` together; hard-coding one OS bakes
  its assumptions into later generations.
- **Private temp dirs for native builds:** parallel jj workspaces share `/tmp`;
  fixed names race. Allocate+export private `HB_TMP` (create it before spawning
  makers — a missing temp root collapses into an empty nonzero failure); derive
  parent artifact paths and child `HB_TMP` from the same getter. Cleanup prefers
  `trash` but falls back to direct removal (trash rejects `/var/folders`).
- **Parity is native fixpoint, not mirror drift:** the proof is byte-for-byte
  self-rebuild through build-fixpoint; retire bootstrap token-diff lints as a
  second source of truth.
- **Active work lives in dots, not a root plan:** once tasks have `.dots/*.md`, a
  root plan is drift. Current verification in `STATUS.md`, memory in `LESSONS.md`,
  ready work in `dot ready`. Completed/landed plans retire their root docs; root
  Markdown is contracts, status, or active work only.
- **On-device validated: this environment IS an Orin (2026-06-27).** Local GPU
  (`/dev/nvidia0`, `nvidia-smi` = "Orin (nvgpu)", compute_cap 8.7), `ptxas` 12.6,
  Tegra `libcuda.so`. Proven end-to-end: checked SAXPY → `PTX-EMIT-SAXPY` →
  `ptxas -arch=sm_87` → cubin → **loaded as a live GPU module via the Habu FFI**
  (`tools/ptx/cuda-load.f`): `cuInit`/`cuDeviceGet`/`cuDevicePrimaryCtxRetain`
  (NOT `cuCtxCreate` — that hangs on the Orin's camera primary context)/
  `cuCtxSetCurrent`/`cuModuleLoad`/`cuModuleGetFunction` all rc 0, valid function
  handle. FFI usage: `DLOPEN-SLOT @`/`DLSYM-SLOT @` give dlopen/dlsym; `ffi-call
  ( argbuf fn -- rc )` loads x0..x7 (≤8 args). Run-mode hangs (use `--load`, not
  `bin/hb file.f`); processes hang on exit unless the module/ctx are released.
- **GPU launch WORKS — the blocker was driver SYMBOL VERSIONING, not the FFI/ABI
  (resolved 2026-06-27).** `cuMemAlloc` returned 201 INVALID_CONTEXT *even though*
  `cuCtxGetCurrent` confirmed the context was current (handle matched the retained
  ctx) — which REFUTED the x18/TLS hypothesis. The real cause: the modern CUDA
  driver's actual entry points are the **`_v2` symbols**; `dlsym("cuMemAlloc")`
  returns a deprecated stub that fails. Using `cuMemAlloc_v2` / `cuMemsetD32_v2` /
  `cuMemcpyDtoH_v2` (the non-versioned setup/launch symbols are fine: `cuInit`,
  `cuDeviceGet`, `cuModuleLoad`, `cuModuleGetFunction`, `cuFuncSetBlockShape`,
  `cuParamSetv`, `cuParamSetSize`, `cuLaunchGrid`, `cuCtxSynchronize`) the **full
  SAXPY launch runs and is CORRECT** on the Orin: y = a*x+y = 3*2+0 = 6.0
  (f32 0x40C00000), matching the CPU golden (`tools/ptx/cuda-launch.f`, PASS).
  The deprecated `cuLaunchGrid`/`cuParamSet*` path (<=8 args) avoids needing an
  `ffi-call` >8-arg extension for `cuLaunchKernel`. NO engine change was needed.
  The eval-matrix data path is OPEN: emit checked kernel -> ptxas -> cubin ->
  load -> launch -> compare golden, all from Habu.
- **Real Triton on the Orin without a reflash (eval matrix DONE 2026-06-27).** The
  GPU driver is BSP-pinned at CUDA 12.6 (`cuDriverGetVersion=12060`); a 13.x driver
  needs a destructive JetPack-7 reflash (don't). pypi's default torch is `cu130` →
  won't load (`driver too old`), and the Jetson torch index `pypi.jetson-ai-lab.dev`
  is DNS-unreachable — BUT `download.pytorch.org` carries `torch-2.9.1+cu126`
  aarch64/cp310, and **cu126 matches the 12.6 driver exactly**, so
  `torch.cuda.is_available()` is True. The generic SBSA wheel has no sm_87 ATen cubins
  (`cudaErrorNoKernelImageForDevice` on `x+x`), but **Triton JIT-compiles each kernel
  for sm_87 at runtime**, so Triton kernels run — just avoid ATen GPU ops (alloc +
  H2D/D2H memcpy only). Result (`docs/eval-triton.md`): scalar Habu-PTX measured
  42.5 GB/s vs Triton 63 GB/s; RCA showed the gap was **codegen vectorization**
  (scalar global loads/stores vs vectorized global traffic), and the checked v4
  tile path now reaches ~63 GB/s parity. Triton catches name/type errors at compile
  but the **stack-discipline class only at runtime** (3/5 battery bugs slipped,
  incl. a missing-store → silent 0.0) where Habu-PTX's checker rejects at author
  time. The thesis MECHANISM is now backed by real-target data; do not revive the
  superseded launch-path-gap explanation.
- **External baseline stays out of the tree (host-lint).** `host-lint` WALK-FILES the
  whole repo and `1 throw`s on any `.py` path; `.md`/`.f` content is not scanned. So
  the Triton (Python) baseline lives as fenced ```python blocks in `docs/eval-triton.md`
  (reproduction reference) with the working scripts in `/tmp`; the Habu reducers
  (`bandwidth.f`, `eval-compare.f`, `eval-device.f`) remain the live column.
- **Model-driven pass@k needs an INDEPENDENT generator, not a curated fixture.** A
  real pass@k/repair number requires a stochastic generator we don't curate (curating
  the bug distribution makes the number a construction). Used independent Claude
  `general-purpose` subagents (k=5/task/target) as the generator; graded each through
  the target's full loop (Triton compile+device-golden; Habu checker+emit+ptxas+device).
  Result (docs/eval-triton.md): SAXPY 5/5 both; softmax Triton 5/5, Habu 3/5→5/5 after
  diagnostic-guided repair (1–2 rounds). Both highly reachable; the differentiator is
  failure MODE — every Habu failure was an author-time static reject with a located
  order diagnostic (`at 'row-store' expected: tile span rowctx actual: span rowctx tile`),
  zero GPU, which drove repair. Honest caveats logged: the softmax pass@1 gap was
  CONFOUNDED by my own prompt mis-spec of ROW-STORE's arg order (real order tile/span/ctx),
  and Triton produced no failures to repair so repair-rounds isn't a symmetric comparison.
- **Subagent grading harness gotchas:** Triton `@jit` rejects `exec`'d source ("should
  be defined in a Python file") — import the candidate as a real module file. Loading
  `maki/eval-device-sm.f` as a library runs its inline test block + `bye`; strip to a
  definitions-only copy for reuse. Normalize generated Habu candidates (strip a leading
  `:`/trailing `;`) before wrapping as `: K ... ;`. ROW-STORE consumes (tile span ctx)
  with the tile DEEPEST — the natural idiom leaves the tile on the stack then appends
  span+ctx.
- **v4 vectorization closes the bandwidth gap to Triton; 63 GB/s is the MEMORY ceiling,
  not codegen (2026-06-27).** The scalar codegen emitted `ld.global.f32` (1 elem/thread)
  -> 42.5 GB/s vs Triton's `ld.global.v2` 63. Added a checked v4 tile vocab
  (`lib/ptx/cg-vec.f` EMIT-*-V4 + `tile-v4.f`: `ld.global.v4.f32`/`st.global.v4.f32`,
  4 elems/thread, same parametric tile types so the SAME SAXPY body certifies — v4 is a
  codegen rep, not a type change) -> 63 GB/s, MATCHING Triton (device-golden PASS). Then
  tried to BEAT it: unrolled grid-strided v4 K=1/2/4/8 (up to 8 v4 loads in flight) is
  FLAT at 63; occupancy is 40x saturated (Orin NX = 4 SMs / 6144 threads); EMC already
  maxed (3199 MHz, jetson_clocks no-op). So 63 is the achievable streaming bandwidth
  (~62% of the ~102 GB/s Orin NX spec), and BOTH targets sit at it. You cannot beat the
  memory system on a memory-bound kernel — "faster than Triton" needs LESS traffic
  (op fusion) or a COMPUTE-bound kernel. Sig gotcha: v4 emit-helper stack sigs must use
  the generic-int token `n` (role names like `base`/`spanrd` bind as fresh type vars and
  the checker rejected at `!`). Device knobs: nvpmodel -m 0 (MAXN) was rejected on this
  unit; jetson_clocks locks clocks but needs `--store` before `--restore`.
- **Rigid-token fix is now a precise, bounded change (located 2026-06-27).** The
  checker instantiates a called word's effect by RE-PARSING its stored signature
  STRING per call (user/prim sigs are stored as text at `USIGS`, checker.f:886;
  re-parsed through `SIG-TYPE`). Type vars freshen per parse (`NMAP-RESET` +
  `VAR-OF`→`FRESH`), but nominal atoms (`MK-ATOM`) keep their literal name, so two
  parses of `extent-n`/`mask-live` produce string-equal atoms — the exact reason a
  constructor's extent/mask is NOT fresh per call (the soundness gap). FIX: add a
  fresh-marker atom convention (e.g. `mask-fresh`/`extent-fresh`); in `SIG-TYPE`/
  `MK-ATOM`, mint a UNIQUE name (base + a global counter) for a marked atom so each
  call's parse yields a distinct RIGID atom that string-mismatches others. Declare
  `GRID-CTX`/`ROW-CTX`/`MK-SPAN` outputs with the fresh marker. Result: two
  independent ctxs get distinct rigid masks → a mixed-mask op rejects (a generic
  op's var requires both equal; distinct rigid atoms don't unify), while one ctx
  bound to a local reuses its single fresh mask → SAXPY still certifies. The
  checker now stores fresh atom templates and renders recorded rigid outputs back
  as distinct templates, so wrapper words preserve equality/distinctness too.
- **Local type inference is already built too (proven 2026-06-27).** Like M2, the
  inference.md feature ("infer bodies, annotate the edge") is operational in the
  shipped checker: untyped intermediate locals (`{: x :}`/`{: g :}`/`{: c :}`)
  certify, the checker infers each from top-of-stack and threads its tokens, and a
  mis-used inferred local rejects with a precise type (an inferred `gridctx` used
  where a `span` is required → "expected span actual gridctx", exit 70). The
  SAXPY/SOFTMAX kernels rely on it (the mask/extent tokens thread through inferred
  intermediates). Only the `{: x:? :}` show-inferred form (inference.md marks it
  "proposal, not implemented") is unbuilt. Two named "Habu features to land" (M2,
  local inference) were already shipped — always probe the checker before sizing.
- **Native helper leafness is a control-flow invariant:** extending `LCFPOP` to
  emit local-frame teardown made it call `LCEMIT`; without saving `x30`, `RET`
  jumped back into `LCFPOP` (`pc=lr=...aa44`) and branch-local definitions hung
  at compile time. Any helper that gains a `BL` gets a real LR frame in the same
  change.
- **Extensible nominal types must be explicit:** `DEFTYPE` registers a copied,
  global nominal role before signatures use it; unknown signature tokens still
  reject. Silent auto-interning would turn typos into distinct "valid" roles.
- **Ground capability claims in the source, not the dot tracker + spec.** The PLAN
  review concluded "M2 (parametric checker) is a large unbuilt gate" from `dot ls`
  (no `m2` dot) + the spec calling it "large", and elevated it to CRITICAL. A
  10-second empirical check refuted it: M2 is fully built+landed in
  `src/core/checker.f` (`SIG-TYPE`/`MK-PARAM` parse `span<...>`, field-by-field
  unify, `render.f` round-trip, `KERNEL:`/`GRID:`/`WHERE`) and works in the
  installed `bin/hb` — a matching parametric sig certifies (exit 0), space/extent
  mismatches reject with field-precise diagnostics (exit 70). A "missing dot" can
  mean *done*, not *unbuilt*. Before sizing a checker feature as a prerequisite,
  run a minimal certify/reject probe through `bin/hb`; the real unbuilt frontier
  was M4 (the tile *operations*: MK-SPAN/LOAD/STORE/SCALE/collectives — absent),
  not M2.
- **Dot hygiene:** use only documented commands — `dot add "T" -d '…'`,
  `dot ready`, `dot ls`, `dot tree`, `dot show <id>`, `dot on/off <id> -r '…'`.
  Mark/close the exact printed ID (don't infer from filenames). `-P <root-id>`
  only when `.dots/<root-id>/<root-id>.md` exists; never `-P`/`-a` a nested id (it
  writes a blocking edge / stray dirs / ambiguous IDs). Quote `-d` (backticks,
  `$`, Forth punctuation). Handoff: leave the dot open, record commit + pending
  edits + definition-of-done in the committed doc, close only after the validated
  commit; add step-level child dots for in-progress work.
- **Keep debugger docs in the agent index:** list `docs/debugging.md` (`.s`, watch
  cells, REPL `step`, breakpoints, `jitdump`, `imgdump`) in `FILEMAP.md`, guarded
  by `tools/filemap-lint.f`, so RCA starts with existing tools.
- **Focused lint/check reruns copy the test load list:** lint tools have non-obvious
  deps (`tools/date.f`, `lib/memory.f`, `lib/vector.f`, `tools/lint/intern.f`).
  Copy the `TEST:SUITE` entry list from `test/gate-stdlib-cases.f` or the tool
  header instead of reconstructing from memory.
- **Use the current gate command, not stale handoffs:** after the Darwin spawn
  F04 macOS validation, the old handoff gate without `test/gate-pool.f` exited at
  `GT-POOL-START`; the `docs/bootstrap.md` command with
  `test/gate-pool.f test/run.f` passed the full native gate.
- **Close platform dots only with target evidence:** Darwin spawn factoring closed
  after real macOS arm64 refresh, source-shape, process/argv/env/cwd, PTY, and
  full native gate passes; Linux evidence alone was not enough for F04.
- **Checked DSL incidents moved into the standard:** the `GE-FILES:`/`CHK-FILES:`
  generic `execute` failure and `tools/gate-json-assert.f` raw `[']` row failure
  are now covered by the checked DSL/quotation rules in `docs/forth.md`.
- **Layout constants need one owner:** load `src/habu/layout.f` before every
  runtime prefix (env, baked REPL, stepper, watch, debug). Refresh once with
  compatibility constants if the installed `bin/hb` needs them, then remove dups
  and prove with the regenerated binary.
- **Retire stale benchmark programs:** the old cross-language LLM matrix already
  answered the active planning question (Habu cost was roughly 8-10x TypeScript/
  Rust on the hard array tail). Do not maintain that harness while GPU/PTX is the
  focus; keep only the status note unless a new publication-grade claim becomes
  active work.
- **Manifest policy is row data plus one validator:** doc and module-note
  requirements in `tools/stdlib-manifest-test.f` stay maintainable when grouped
  as checked rows; branch ladders hide policy drift and make adding requirements
  look like control-flow work.
- **stdlib gained `ffi`/`float`/`fmt`:** `lib/ffi.f` (typed `DLOPEN`/`DLSYM`/
  `CALL0..6`/`>CSTR` over the `ffi-call` trampoline), `lib/float.f` (`STR>FLOAT`
  decimal→f64 + `POW10`), `lib/fmt.f` (`SB-U`/`SB-INT`/`SB-FIX` builders +
  `U.0`/`F.N`). Registering a new lib = `std.manifest` rows for every public
  colon word (match `tools/public-signatures.f` output *exactly* —
  `TRUSTED:`/constants/`.0`-style names are NOT extracted, so they get no row), a
  `FILEMAP.md` row, a `TEST:SUITE` entry, and `TRUSTED.md` rows for any
  `TRUSTED:`. The doc-contract check is a curated spot-list, not per-module.
- **Warm image entries cannot hide dependencies behind include:** a normal
  `tools/check.f` can `include tools/check-core.f`, but warm images load entry
  files through their baked source map and the include boundary can fail or crash.
  Bake the core into the warm image when it fits, then load a no-include
  `*-main.f` entry; otherwise pass core+entry explicitly with `WR-TOOLS-LOAD2`.
- **Fixed pool slots must be reserved before dynamic starts:** starting a
  dedicated slot after a general phase can reset an active slot, orphan the child,
  and leave `GT-POOL-LIVE` permanently high. `GT-POOL-START-SLOT` now fails
  closed on active-slot reuse; start fixed-slot warm runners before free-slot
  phases that might claim their indexes.
- **Habu's locals/loop discipline costs first-time iteration:** porting real code
  (Odin's tegrastats/netpbm parsers to `../odin-habu`) hit the same walls
  repeatedly — no local bind after `exit`, a `begin/while` condition may only add
  a flag, a no-`else` `if` must be stack-neutral, `i`/`j`/`k` are reserved loop
  words. The rules now live in `docs/forth.md`; once known, later ports needed
  ~zero checker iterations. The win is verification, not authoring speed.

## VCS

- **Diff gates must scan locals:** typed definitions are not enough; added `{:
  ... :}` groups need typed locals unless they are the documented
  role-preserving exception from `docs/forth.md`. Use the Habu-native
  `tools/typed-local-diff-lint.f`; manual `rg` scans are too easy to skip or
  misread.
- **Repo edits go through patches/Edit:** even mechanical/one-line changes, so
  broad rewrites, duplicate defs, and shell-expanded capture groups stay reviewable.
- **Commits are proof checkpoints:** run the path-specific Habu gate before
  `jj commit`; a skipped or failed gate stays uncommitted or becomes a blocker
  dot, never a "commit now, fix later" checkpoint.
- **Don't parallelize VCS status:** `jj st` and `git status` race on
  `.git/index.lock`; run index-touching Git/JJ commands sequentially.
- **Pushes reject conflicted ancestors:** a clean worktree + green gate don't clear
  jj conflict metadata; check the pushed range for `conflict` and resolve the
  earliest one first.
- **History filters must include JJ refs:** after removing generated outputs,
  verify `git rev-list --objects --all`, not just `master`; colocated JJ keeps
  blobs via `refs/jj/keep` and `.jjconflict-*`. Ignore generated output by
  shape (JSON/JSONL/logs/reports), not run name, so reruns can't enter a change.

## Runtime, Codegen, AOT

- **Warm snapshots must not re-run the cold prefix:** restored images already
  carry support words/signatures. Mark snapshot boot (`SNAP-CELL`) and skip the
  source prefix, or reloaded core words can shadow warm support words and bind
  trusted signatures to the wrong runtime code.
- **Snapshot persistent stores before image write:** compact checker stores
  (`USIGS`, `NORET`) back into snapshotted boot arrays and reset only transient
  token buffers. Never snapshot pointers into build-time anonymous mappings.
- **Instruction patching flushes the patched line, not `[addr, CP)`:** `patch32`
  can patch at `cp@`; range flushing through `CP` is empty there and leaves stale
  instructions at cache-line boundaries.
- **Snapshot images relocate only engine-text refs:** fixed VAs keep dict/data
  addresses valid; rebase engine-text call chains + seed-prim slots, restore live
  per-boot cells (argv/envp, RBASE, S0, wordlist, hook) after copying. Accept a
  trailer only when `region-len + data-len` ends exactly at the trailer offset
  (magic also appears in code). Output paths use guarded `PATH0` buffers (a small
  buffer overwrites trailer magic → silent cold image).
- **`evaluate` is re-entrant state:** `B-EVAL` saves INP/INE, SP, XDS, CP, NDICT,
  DP, return addr, then branches to the interpreter top; clean end keeps defs,
  error restores compile state and sets `EVALERR`.
- **Fixed DATA cells require layout audits:** placing `EVAL-FRAME` in a free-looking
  gap overlapped regalloc tables → invalid ARM64 fields. Centralize fixed header
  offsets, verify non-overlap. x20 is the engine DATA base, not scratch — no
  runtime/debug helper reuses it before the next `DATA` access.
- **Runtime spans use OS-backed memory, not bigger DATA caps:** static dict storage
  can use DATA; runtime source/report/JSONL/capture buffers size from measured
  bytes via `MEM-ALLOC-64K-SPAN`/`lib/memory.f`. Derive the live base through
  `data-base`, never a duplicated numeric DATA address (a gen-1 stage runs old
  startup while interpreting new source).
- **Warm composition finds hidden DATA buffers:** `diag-origin`, `signature-lint`,
  `trust-lint`, and `icode` failed only when loaded together. Runtime file
  buffers, scan tables, assembler code, and fixup tables belong in OS mappings;
  keep dictionary DATA for persistent definitions and small cells.
- **Warm JSON parser storage is runtime scratch:** `tools/json.f` parse tables,
  string storage, and writer/error buffers are part of the warm checker prefix;
  keep them in lazy `lib/memory.f` mappings or the 67 KiB warm trust sidecar can
  fail before diagnostics run.
- **Batched warm tools expose static scratch debt:** after xref/checker state
  growth, static 256 KiB buffers in `build-fixpoint`, `hb-build`, `repair-packet`,
  `json-only`, `repair-schema-doc-test`, and `stdlib-manifest-test` failed as
  silent rc 76 under warm suites. Large source, capture, doc, and JSON buffers use
  lazy `lib/memory.f` mappings; update the owning load list, not the batch shape.
- **Code/dict region guards (a lone `:` failing closed):** check `cp@ dbase@ -`.
  At/past `REGION - $4000` → code-region guard (remove one-use compiled fixtures,
  then grow `REGION`). Well below + rc 77 → dictionary guard: grow `DICT-CAP`,
  `CFSTK-OFF`, `DICT-SIZE` together (native + bootstrap mirrors), then recheck code
  headroom (larger `DICT-SIZE` moves code up). Undefined tokens near a huge word =
  BODYBUF pressure: factor the colon body into section words. Capacity bumps are
  layout work — prefer removing one-use signatures first.
- **AOT compaction needs an old→new map:** removing call-stencil padding is safe
  only when every PC-relative source (`B/BL`, cond branches, `ADR`) is remapped and
  range-checked; keep mapper cursors separate from copy cursors. Capability checks
  need a source-level pass (inlined data-space prims like `here` vanish from
  closure scans). Keep the >256-word closure fixture above the old threshold.
- **JIT inliner must reject PC-relative branches:** byte-copying a small prim body
  with an internal branch preserves the old target (compiled `epoch-seconds`
  branched back into the seed prim, returned 0). Branch-bearing bodies compile as
  calls unless relocated.
- **Registry/name growth fails closed at build time:** process prims overflowed the
  96-entry seed registry and corrupted the stage image; keep `REG-PRIM`
  capacities named/checked. `EMIT-DICT` encodes names > `DNAME-INL` out-of-line
  (inline-only corrupts `DREC`). Dictionary names are strings with flags above the
  length field, one decode path (low-bit flags recreate a 255-byte cap).
- **Factor emitters before growing them:** `EM-COMPILE` is near the body-capture
  limit; new compile paths become named emitter helpers with TRUST rows (a
  body-capacity exit during self-build is a structure signal). Label locals must
  match label values exactly (an extra name consumes stale generator state and
  crashes later). Values surviving an emitted `BL` need explicit stack saves or a
  documented callee-preserved contract — `EMIT-BCAP` clobbers `x16`, so a quote
  start kept there made the maker copy from `0x40`.
- **Dispatch factoring starts with semantic groups:** split long native/recovery
  compiler dispatch chains into checked helper groups before adding a row DSL;
  source-shape tests guard helper presence, call graph, and removal of old chains.
- **Emitter punctuation caused stage2 false trails:** a dropped comma changes the
  token (`BL,` vs `BL`) and surfaced only as a terse undefined-token exit in
  generated stage2; the durable emitter-source rule now lives in `docs/forth.md`.
- **Mirrored codegen lands twice:** `src/habu/` and `bootstrap/cg/` are one
  contract, two sources — factor both, prove native fixpoint + Gforth recovery.
  They may use different assembler-word vocabularies for the same instructions;
  preserve each file's opcode spelling and run the native engine gate + bootstrap
  codegen tests. Move-wide (`LVMOVK`) refactors need compiled-literal cases (zero,
  all-ones, MOVZ/MOVK, MOVN/MOVK) — top-level parser checks don't prove the JIT
  materializer. Recovery seed prims drift unless grouped + bootstrapped (a native
  fixpoint alone doesn't prove no-binary recovery). Bootstrap metadata parsing
  must fail closed (`BODY-ARITY`/`EFFECT-FLAGS` rethrow under the diagnostic
  boundary, never default to arity 1 / flags 0).
- **Encoder factoring needs value fixtures + shape guards:** add executable
  fixtures with known instruction words for every encoder touched; source-shape
  scans only prove the file stayed factored.
- **Target image byte cursors are one contract:** ELF, Mach-O, signing share
  `MBUF`/cursor/endian-patch vocabulary under executable fixtures + shape guards;
  target files hold only format-layout policy.
- **Trust-call emitters name the protocol:** push one data cell, push the signature
  pair, call x11 with LR saved — not raw inline `G-PUSH`/`BLR`. `TRUSTED:` bodies
  don't take locals reliably; factor checked helpers and keep the body to the
  asserted primitive op (parsing stops at `{:` otherwise). Trust audit dates follow
  the gate's epoch/UTC day (near local midnight a local date can be "future").
- **Useful register lint needs contracts:** model callee returns/preserves,
  no-return exits, routine boundaries, syscall clobbers, LR/SP; raw
  write-before-BL/read-after-BL rules are mostly false.
- **Split open vs read failure labels:** `EMIT-SOURCE-READ` shared one label;
  on open failure x12 still held the path → `close` on a non-fd. Distinct labels
  for no-resource-acquired vs acquired, even with the same exit code. Scratch cells
  are caller state — helpers don't reuse a caller's loop-index var (`SOURCE-LINE-END`
  clobbered the outer `SOURCE-I`); branch paths preserve loop indexes.

## Runtime And REPL

- **Process capture lifecycle has one owner:** keep fd setup, nonblocking
  probe/drain, stdin write, timeout poll, cleanup, and finish in `lib/process.f`;
  argv/env/cwd layers prepare state only. Duplication made every capture variant
  a stack-juggling audit.
- **Linux spawn needs an exec-failure handshake:** `clone` success ≠ `execve`
  success. `PROC-SPAWN-IO` uses a close-on-exec error pipe: child writes one byte before
  exiting on `chdir`/`dup2`/`execve` failure; parent reads EOF = success, or reads
  the byte, reaps, returns `-1`. Copy the fd to x0 before reusing that register for
  the marker byte. Else checked `PROC-SPAWN-IO` returns a pid for a missing exe.
- **Baked REPL needs explicit hook boundaries:** the snapshot prepends `0 set-check`
  then reinstalls `HOOK` before user input. The tty bundle emits `0 set-check`
  before `repl-term.f`/`repl.f`/watch/stepper/debug and then only `' HOOK
  set-check`; defining a second `HB-CHECK-HOOK` collides once explicit
  duplicate-definition enforcement is active. Core fixtures (`src/core/sha256.f`)
  don't disable checking themselves; put `0 set-check` in the harness.
- **`--load` leaves stdin as tool data:** so a post-load probe piped to fd0 doesn't
  run — put capacity probes in an explicit loaded source file when measuring
  `here`/metadata. Test load lists factor into `TEST:SUITE … TEST:END-SUITE`
  blocks with short lines (long physical lines hit the reader buffer).
- **PTY behavior needs a real pty harness:** `script(1)` interleaves echo/output;
  drive a pty directly and poll for exit when testing prompt, raw mode, history,
  Ctrl-C/Ctrl-D, async termination.
- **`close` is `( fd -- )`, no status:** modeling `( fd -- rc )` leaves a phantom
  cell rejected at branch joins; for atomic output write a temp file and commit
  with fallible filesystem words.
- **Native crashes need debugger state first:** for SIGSEGV/SIGILL/DATA/stack
  corruption set breakpoints, step, inspect data-stack + watch cells; extend the
  debugger before print-marker probes. Stale binaries hide fixes — remove output
  artifacts before generate-then-run tests, never silence the build.
- **Stdin/Forth byte fixtures:** `s" …\n"` keeps the literal `\` and `n`; use a
  byte buffer (`10 c,`/`STR-LF`) for newlines. `\\` is a different token from the
  comment word `\`. Checked helpers are defined before use (a forward ref can
  surface later as an unpublished word).

## Darwin And Syscalls

- **Raw Darwin syscalls are not libc:** `posix_spawn` (244) takes the private 5-arg
  kernel ABI `pid*, path, adesc, argv, envp`; `wait4` status is `(status>>8)&0xff`;
  check carry + errno-in-x0. Initialize ALL expected zero/null arg registers before
  `svc` (`gettimeofday` returned `EFAULT` from stale x2 after a `write`). Syscall
  output buffers use audited DATA scratch, not live `sp`.
- **Process redirection uses XNU spawn descriptors:** empty file-action blobs are
  invalid (pass null when no remap); mark parent-only pipe/pty fds close-on-exec
  before spawning. PTY = `/dev/ptmx` + ioctl, not `forkpty`/`openpty`.
- **`F_SETNOSIGPIPE` still needs normalized failure:** closed-pipe `write` reports
  `EPIPE=32` which can collide with a valid byte count; normalize carry in the
  syscall prim, validate counts, keep a 32-byte closed-pipe regression.
- **Darwin time is not a syscall:** `clock_gettime*`/`mach_absolute_time` are
  libSystem/commpage; the no-libSystem clock reads `CNTVCT_EL0`/`CNTFRQ_EL0` and
  converts via quotient/remainder (avoid `ticks*1e9` overflow).
- **LC_MAIN gets argc/argv/envp in x0-x2** (no kernel start-stack); capture at entry
  and restore after snapshot boot.
- **Recursive dir walks need per-depth buffers:** `getdirentries64` records are
  batch-local; index dirent buffers, offsets, lengths, cookies, fds by depth — even
  a global current-record pointer is unsafe (the child walk overwrites it).
- **Share filesystem traversal mechanics, not policy:** `WALK-FILES` skips repo
  metadata while `REMOVE-TREE` must delete `.dots` and unlink symlinks first.
  Factor open/read/record/child-path/close helpers in `lib/fs.f`; keep deletion
  choices in `lib/fs-mutate.f`.
- **Symlink deletion lstats first:** `EXISTS?`/`FILE?`/`DIR?` follow symlinks and
  broken links look absent; `REMOVE-TREE` tests `SYMLINK?` before existence/type.
  Repeatable fixtures use `MAKE-DIRS` or clean the tree before `MAKE-DIR`.
- **Path syscall tests use stable NUL strings:** private path-copy helpers can make
  a primitive smoke test fail with `EFAULT`, hiding whether the ABI or fixture glue
  is wrong. Same-typed string pairs (`ptr u8 n` path vs stdin bytes) need an
  order test — the checker can't distinguish them.

## FFI / GPU (PTX)

- **FFI needs almost no new ABI machinery:** an AAPCS64 C-call is a non-leaf
  `FPRIM` (gets the x30 frame) that `G-POP`s fn + arg buffer, loads `x0..x7`,
  `BLR,`, `G-PUSH`es `x0`. `XDS` is `x19` (AAPCS64 callee-saved) so the C callee
  preserves the data stack free. The CUDA Driver surface (except `cuLaunchKernel`'s
  9th–11th stack args) is integer/pointer-only, so the float scalar rides in the
  `kernelParams` buffer — no v-register handling. `ffi-call : ( ptr a n -- n )`,
  fail-closed.
- **Dynamic-ELF FFI is checked emitter code:** `bin/hb` is a dynamic ET_EXEC
  (`PT_INTERP=/lib/ld-linux-aarch64.so.1`, `DT_NEEDED libc.so.6`, SysV `.hash`,
  `R_AARCH64_GLOB_DAT` into a fixed-vaddr GOT, `DF_BIND_NOW`, no PLT) so ld.so
  fills `DLOPEN-SLOT`/`DLSYM-SLOT` (`ptr a`, read with `@`) for `ffi-call`. Place
  the R+W segment (.dynamic+GOT) at a FIXED high vaddr so slot addresses are
  compile-time constants independent of code size. Both the AOT (`BUILD-ELF`) and
  snapshot (`BUILD-SNAP-HDR` + `snap.f`) image paths must go dynamic — the
  self-host fixpoint, not just `BUILD-ELF`, is the acceptance test.
- **no-crt dlopen works:** glibc ld.so initializes libc.so enough that a
  no-startfiles binary (own `_start`, no `__libc_start_main`) can `dlopen` libcuda
  + call `cuInit` (proven on the Orin with `gcc -no-pie -nostartfiles`).
- **`cp@` is only stable inside a compiled word:** to emit a runtime stub (a C-ABI
  leaf for `ffi-call`), write at `cp@` via `patch32` and call from inside `: WORD ;`.
  The interpreter compiles each top-level line into a transient buffer at `cp@`, so
  a top-level `cp@ patch32` clobbers the executing line (the `add` ran, `ret`
  landed in junk → `SIGILL`). Engine-suite proofs pass via `--load` yet `SIGILL`
  via the gate's stdin REPL — always reproduce engine-suite changes through
  `bin/hb --repl < test/engine-suite.f`.
- **Verify emitted primitive bytes statically:** compute the exact ARM64 encodings
  and `grep` the on-disk `bin/hb` for the contiguous byte stream (ASLR slides the
  `[']` xt; file bytes are fixed) — proves codegen without a runtime call.
- **M3 has two proof levels:** `tools/ptx/saxpy.f` emits `.version 8.3`/
  `.target sm_87` PTX and `tools/ptx/ptxas-smoke.f` proves `ptxas` assembly on
  Orin without committing `.ptx`/`.cubin` artifacts. GPU launch + CPU golden is
  not an M3 completion proof until the Habu CUDA Driver harness (M1d) launches
  the cubin; the old `/tmp/saxpy_run.c` C smoke was useful evidence, not repo
  infrastructure.

- **M4a crux — trusted span constructors need FRESH extent-token minting
  (proven 2026-06-27).** M2's parametric types are built and work, but the M4
  constructor `MK-SPAN ( ptr u32 -- span<...,extent-n> )` cannot just use the
  nominal atom `extent-n`: a probe (`TRUSTED: MKS … span<…,extent-n>` twice, fed to
  `NEEDSAME ( span<…,extent-n> span<…,extent-n> -- )`) CERTIFIES (exit 0), so two
  *independent* spans wrongly share `extent-n` — violating ptx-sketch.md ("a lone
  MK-SPAN yields a fresh N that unifies with nothing"). The model split: kernel
  *signatures* use nominal atoms (`extent-r`/`extent-c`, equal-by-name, to ASSERT
  agreement), but *constructors* must mint a fresh extent per call (`MK-SPAN=` is
  the explicit share-one-fresh-token form). The landed design: a field
  **type-variable** (`span<…,e>`) does NOT work — fresh unification
  vars unify freely, so two independent spans could always be unified "equal,"
  which is exactly the unsoundness to prevent. The constructor must mint a
  per-call-fresh **rigid (skolem) extent token** that unifies only with itself;
  `MK-SPAN=` mints one rigid token stamped on both outputs. A kernel needing equal
  extents is polymorphic in the extent and requires the SAME token twice, so
  passing two DISTINCT rigid skolems forces them equal and FAILS (reject), while
  two `MK-SPAN=` outputs share one skolem and pass. This is a genuine checker
  extension (per-call rigid-token minting at trusted constructors), not just word
  signatures. Constructor signatures now use `fresh-extent-*` / `fresh-mask-*`
  templates; `GRID-CTX`/`ROW-CTX` mint fresh masks and the engine gate proves
  both direct and recorded-signature reject paths.

## Linux AOT / ELF

- **Linux AOT gates parse ELF structure:** inspect ELF64 program headers and
  validate the executable `PT_LOAD` segment; Mach-O text-size thresholds aren't
  portable constants.
- **Instruction disassemblers read instruction width:** `DISASM` loads one ARM64
  u32 at a time; a u64-load-and-mask can cross a 4-byte mmap fixture end.
- **`die` modeling belongs to the Forth standard:** the Linux AOT failure came
  from treating `0 0` as a fake string and mixing `throw`/process-exit control
  effects; the lasting rule now lives in `docs/forth.md`.

## Diagnostics & Benchmarks

- **Diagnostics are an API:** JSON errors carry `schema_version:1`, source spans,
  verdict, word, token, expected, actual; wrappers keep valid JSON object lines and
  fail nonzero on rejection. Source origins are wrapper-owned (definition-relative
  spans; inject origin markers, keep them out of user bundles). Repair/diagnostic
  rows keep a source-preserving effect field (`R x -- R x`) beside the normalized
  one; `fix_return_stack` only when the data stack already matches (a bad `>r` that
  drops a declared output is `add_producer` first).
- **JSON quoting/storage split:** the `s"` quoting rule moved to `docs/forth.md`;
  the infra lesson is that row artifacts exceeded fixed builders, so large
  prompt/response/replay fields need chunked Habu emitters or OS-backed storage.
- **Check phases must be silent:** `hb` can emit checker diagnostics yet exit 0 for
  a loaded file; live drivers treat ANY stdout/stderr from a check-only child as
  rejection. Expected-throw fixtures stay quiet (opt-in row/error reporters at the
  CLI boundary) so negative tests assert throw codes without `FAIL` lines.
- **Core/CLI lint splits need output routing:** standalone lint CLIs may write
  findings to stdout, while `tools/check.f` must surface the same findings on
  stderr. When moving a lint in-process, make the checked core accept/configure
  its output fd and add check-wrapper tests for text and JSON stderr routing.
- **Don't pull unchecked parsers into checked tests:** `tools/json.f` is an
  unchecked `catch` boundary; checked reducers use small checked interface fixtures
  for the checker gate and runtime tests with the real parser until the typed
  `catch` dot lands. Checker-only stubs are contracts — update the stub in the same
  change as any consumed API move (runtime tests with the real module don't prove
  the gate models new constants/effects/codes).
- **`tools/check.f` modes:** `--all-errors` checks top-level defs independently, so
  on bundled stdlib it falsely flags deps as undefined and multiplies gate time —
  run the fast fail-closed bundle check first, all-errors only after failure or
  explicit request; use `--json-errors` for full driver bundles. Don't accidentally
  type-check a CLI main: the runner executes the final source after checking, so
  scripts with a top-level `MAIN` need a no-main fixture or safe argv/env.
- **Benchmark axes stay separate:** trial pass, task pass@k, repair rounds, wall
  time, generated-token cost (a proxy, not hidden reasoning). Reports are
  deterministic (evidence-derived text, no wall-clock stamp; provable with `cmp`
  before archiving outside git). Habu-only vs cross-language claims use distinct
  artifacts (Forth-only run = repair/replay behavior; 600-row array run = "best LLM
  target"). Stale-status lint ignores fenced evidence blocks, keeps prose strict.
- **Stateful scanners split at cursor phases:** `tools/stale-status-lint.f` became
  reviewable when `SS-COUNT-LINE?` delegated cursor advance, digit runs, ratio
  tails, whitespace skipping, and keyword checks to typed helpers. Keep fixtures
  around the boundary behavior: short counts, embedded alnums, partial ratios,
  case-insensitive keywords, fenced blocks, and long files.
- **Live sweeps resume + enforce coverage by identity:** model calls/repairs can
  die before a row; resume on `(model_id, arm, task_id, trial)` without duplicating,
  record expected identities during the run, and fail before report generation if
  any row is missing. Required ablations (structured vs raw vs blind feedback) are
  default with tested opt-out — env-gated arms get silently skipped. Use
  `*-OUTCOME` process APIs (`PROC-OUTCOME>RC`) so a timeout still emits a row, and
  include task/model/arm/trial/child-rc in missing-row errors.
- **Codex benchmark candidates come from `--output-last-message`:** `codex exec
  --json` stdout is event streams (tool output, truncation) for token accounting
  only; read the last-message file as the candidate, and use `--cd` to a clean temp
  dir + clean `CODEX_HOME` + disabled apps/plugins (a smoke prompt fell 29k→11.5k
  input tokens; rows still report output tokens only).
- **Background Codex scouts need explicit stdin/output:** redirect stdin from
  `/dev/null`, pass `--output-last-message /tmp/name.out`; stdout logs alone are
  unreliable evidence.
- **Dogfood benchmark hot paths:** per-call glue (model-response parsing) is
  Habu-native; host parsers hide missing Habu JSON/string/file primitives and
  weaken the LLM-native proof. LLM helper surfaces match validator surfaces — if
  the validator accepts `A-ARGMAX`/`A-PREFIX-SUM!`/`A-RUNMAX!`, the prompt names
  those exact helpers (else it rewards stack gymnastics).
- **Schema-v2 changes touch every fixture + spawner:** rows with
  `diagnostic_count>0` need `repair_class_stats` summing to the row total with
  preserved `first_round`/`first_order`; report fixtures build full rows by hand;
  CLI runner `--load` lists and doc/dot verification commands stay synced when a
  reducer gains a dependency (`expanded-report.f`→`validate-results-lib.f`,
  `tools/json.f`→`lib/memory.f`). Prove with the exact failing child fixture before
  the full gate. Report reducers use dedicated scratch cells (`RR-I/J/K` get
  clobbered by nested helpers); a checked reducer exposed a `RR-RATIO.` stack leak
  that truncated a table — add row-count regressions and `cmp` regenerated reports.
- **Doc contract fixtures need stable anchors:** when `grep -F` gates API prose,
  keep the phrase contiguous or assert a shorter stable substring; line wrapping
  hides a present contract. LLM stdlib examples cite `lib/std.manifest`,
  `docs/stdlib.md`, runnable `examples/` — not invented API.
- **Public-signature rows:** `tools/public-signatures.f` records only
  `: WORD ( in -- out ) …` with locals AFTER the effect comment; checked `lib/*.f`
  defs (even internals) are captured, track as `active` rows. Effect-drift compares
  full normalized tokens (returning on first shared whitespace made `n --` equal
  `n -- n`).
- **Capacity-sensitive bundles:** keep narrow source-shape/fixture DSLs in their own
  small `TEST:SUITE` and task-specific byte/source builders in the owning
  test/driver until multiple users prove shared surface — appending to large
  live-driver bundles surfaces as an unrelated rc 76 capacity failure. Load
  transitive deps in child bundles (`lib/build.f` needs `lib/process.f` for
  `PROC-RUN-RC`). Large native tool bundles (lint tables + `json.f` + big buffers) can
  corrupt reads — lean standalone reader + distinct scratch vars; stream large
  JSONL in chunks, reserve fixed buffers only for bounded summaries.
- **Source-shape checks distinguish code from quoted code:** generated builders may
  contain strings such as `s" 0 set-check"` that are intended runtime source.
  Tests that ban generated check-off lines should match newline-delimited source
  lines, not raw substrings.
- **Build helper fixtures stay split by boundary:** composing bootstrap-codegen,
  warm-image, build-fixpoint, hb-build, and codesign tests in one `hb` image hit
  rc 76 before `hb-build-test.f` started, while each boundary group passed alone.
  Keep `bootstrap-helper-fixtures`, `build-fixpoint-fixtures`, and
  `hb-build-fixtures` separate.
- **Captured gate children need heartbeat polls:** a silent child looks hung if
  `poll` waits the full timeout; gate capture loops poll in heartbeat slices,
  print label-only wait lines, keep child stdout/stderr for failures. Progress
  must exist at every blocking layer — a top-level `PROC-WAIT-RC` blinds the user even
  if children print heartbeats.
- **Gate heartbeat capture has one owner:** `lib/test/runner.f` owns
  progress-aware drain/flush/stdin capture. Gate files set up phases and
  assertions only; direct `PROC-PFD`/poll loops there recreate process debt.
- **Habu-spawned `hb` children need explicit env:** `RUN-ARGV-CAPTURE` doesn't
  inherit env like a shell; use env-aware process APIs + `PROC-ENV-INHERIT-MISSING`.
  Process capture buffers size for isolated jj workspace paths (not the short main
  checkout) or valid forks throw `E-PROC-TRUNCATED`. Split setup vs runtime
  timeouts (`DS-HB-TIMEOUT` vs `DS-RUN-TIMEOUT`) so a checker/build subprocess
  isn't timed out by a runtime-loop budget. Live-row extras (`runtime`) are set
  after `DS-LR-PASS`/`DS-LR-FAIL` (`DS-CONFIG-LR-COMMON` resets row fields).
- **Core/wrapper splits update every child load list:** when a CLI tool gains a
  reusable checked core (`tools/diag-origin-core.f`), update the wrapper tests,
  build drivers, gate suites, benchmark checker builders, docs, and filemap in
  the same change. Missing transitive deps (`lib/errors.f`, `lib/string.f`) showed
  up as child rc 70 before the parent test could explain the failure.
- **Checker-loaded cores borrow scratch buffers:** moving a CLI scanner in-process
  under `tools/check.f` must not duplicate large static file/string buffers.
  `trust-lint-core` initially plus `check.f` buffers corrupted `SCRIPT-ARGC`
  before parsing; expose caller-supplied scratch buffers and let wrappers own
  standalone storage.
- **All-errors keeps definition isolation, not wrapper spawning:** split
  `tools/check-all-errors-core.f` from the CLI and call it in-process from
  `tools/check.f`, but keep the per-definition `bin/hb` children: each generated
  checked definition may fail independently while later diagnostics still need to
  be collected. Borrow checker buffers for capture; wrapper-only static buffers
  stay outside the composed checker bundle.
- **`stage2-src` cap is a builder contract:** AOT maker generation can exceed the
  256 KiB stage2 reader for tiny user source; reproduce with `hb-build` child
  output, raise the named cap deliberately, keep fail-closed overflow.
- **Source scanners and lint cores exposed standards gaps:** substring source-use
  matches, same-type numeric buffer arguments, and unchecked shared lint cores now
  have durable rules in `docs/forth.md`. The remaining lesson is to keep bulky
  exact-token scanner helpers out of base bundles unless their driver needs them.
- **Commit is a gate, not a checkpoint:** the project rule now treats `jj commit`
  as blocked until changed Forth diffs are scanned for definitions and unchecked
  boundaries, exact owning `bin/hb --load ...` paths are checked, and boundary
  tests/dots exist. This prevents "commit first, typecheck later" drift.
- **Gate phase files need their loader prefix:** `test/gate-stdlib.f` is not a
  standalone script; running it without the documented `docs/bootstrap.md`
  `--load` prefix omits `lib/process-env.f` and fails at
  `PROC-ENV-INHERIT-MISSING` before testing changed code. Use the exact native
  gate command for commit evidence.
- **Warm gate runners are harness artifacts:** bake side-effect-free phase libs
  into a runner keyed by the runner source and seed image, then pass
  `HABU_UNDER_TEST` as runtime env for candidate checks. Baking the runner from
  the candidate on the critical path regressed the full gate to 98.469s; starting
  the runner in the early pool keeps the gate at 80.467s.
- **Warm-image tails mirror builder deps and trailer layout:** warm snapshot
  sources that append `src/os/*/{elf,macho}.f` must first append ARM64
  assembler/code-buffer sources and target/shared layout
  (`src/arch/arm64/{asm,icode}.f`, `src/os/<target>/layout.f`,
  `src/habu/layout.f`), or child snapshots fail at `ASM-LEN`/`CODE-OFF`. The
  snapshot trailer must remain the final 40 bytes of the rounded text segment;
  absorb alignment padding into the copied DATA span instead of writing zeros
  after the trailer, or startup misses `SNAP-CELL` and replays the cold prefix.
- **Stage2 source input is runtime scratch:** a refreshed snapshot `bin/hb`
  starts with substantial DATA already occupied by the checked prefix. Keeping
  stage2's `$80000` source buffer as `here ... allot` combines with `CODE` and
  `MBUF` and trips the silent native `DP-CHECK` guard on the second refresh. Use
  the same primitive-facing `$1002` anonymous/private `mmap` convention as
  `lib/memory.f` for that buffer; raw Linux `$22` is for emitted startup syscalls,
  not the public `mmap` primitive.
- **Phase-token and row-sealing fixes moved into the standard:** the `asm`/`img`/
  `snap` ordering and implicit-row underflow lessons are now checker/type-model
  rules in `docs/forth.md`.
- **Emitter hard-hook cutovers keep roles until raw opcodes:** checked public
  helpers should retain nominal roles (`reg`, `ptr bool`, `ptr a`) and erase only
  at raw mnemonic/immediate boundaries (`REG>N`, narrow trusted fixed-VA numeric
  helpers). The CF-ENTRY cutover worked by keeping dispatchers checked and moving
  only raw `execute` into `EM-HXT-EXECUTE`; whole trusted dispatcher bodies with
  scratch cells crashed during `EMIT-FORTH`.
- **Benchmark driver buffers are shared infrastructure:** the LLM drivers route
  raw model text, repair packets, bundle output, and checker captures through
  `DS-*` spans. Keep those spans OS-backed and capacity-checked at the shared
  helpers (`DS-OUT-TEXT!`, `DS-READ-OUT-FILE`, `DS-HB-CAPTURE-MS`) so one arm
  cannot silently bias pass/reject accounting with a local fixed-buffer throw.
- **PTX toolchain proof has two gates:** first, Habu must emit header-complete
  PTX and `ptxas` must assemble it; second, the CUDA Driver harness must launch
  the cubin and compare CPU golden. On `zed`, `ptxas` exists at
  `/usr/local/cuda-12.6/bin/ptxas` but is not on `PATH`.
- **Composable gate helpers cannot own large static buffers:** loading
  `tools/hb-build-lib.f` ahead of `test/gate-build-common.f` tripped the native
  data-space guard at the common executable read buffer before any new checked
  helper ran. When a gate helper may compose with other checked tools, model
  file/image storage as runtime `lib/memory.f` allocation sized from `FILE-SIZE`;
  keep always-resident `create ... allot` buffers for small fixed metadata only.
- **`STATUS.md` verified date is UTC for the gate:** `stale-status-lint` uses the
  native `DATE-NOW` UTC date unless an explicit test date is passed. Do not
  update `Last verified` to the operator's local date during a late-night run;
  the full gate will reject it even if a manual lint with a local date passes.
  Trust audit dates follow the same rule through `trust-lint`.
- **Gate phase cuts must move coverage, not drop it:** AOT-negative was slow
  because it repeated checker/signature/AOT-lint failures through full
  `hb-build` children. Keep one CLI boundary for hb-build-only behavior such as
  closure-limit failure, and move semantic assertions to their owning checked
  tool tests (`check-test`, diagnostics, `aot-lint-test`, `hb-build-test`).
- **Do not spawn assertion tools from fixture loops:** repair-hints spent most
  of its time re-invoking `gate-json-assert` for JSON schema/class checks after
  each checker rejection. Split assertion tools into checked cores plus thin CLI
  entries, load the core in the owning fixture, and keep subprocesses only for
  the boundary under test.
- **Gate dependencies are per phase, not global:** waiting for every warm image
  before launching every worker serialized independent builder long poles behind
  unrelated tool images. Start phases as soon as their own prerequisites exist
  and only export warm-tool env to phases that actually use it.
- **Gate pool defaults need full-load measurement:** isolated slices improved
  when the stdlib nested pool rose from 2 to 4, but the full gate only improved
  after measuring outer-pool contention on the 4-thread Orin. Keep a higher
  max for explicit overrides, but choose the default from documented full-gate
  timings, not from a single slice.
- **Warm-image trust export is a batch artifact:** `public-signatures --trust`
  already accepts many files, so warm-image baking should export all support
  signatures in one child and size the runtime capture buffer for the measured
  batched output. Per-file export children hide startup cost and make warm
  phases look inherently slow.
- **Warm trust preludes expose only standard signatures:** a word like
  `tools/date.f` `PARSE-YMD` is invisible to `public-signatures --trust` when
  its `( in -- out )` comment appears after `{: :}` locals. Warm-loaded CLI
  wrappers then fail before runtime. Keep the effect immediately after the word
  name, and pass cross-image buffers as real `ptr u8` values from
  `MEM-ALLOC-BYTES`, not raw `create` storage.
- **Semantic xref belongs in the image:** when investigating dictionary or call
  ownership, prefer baked native Forth words (`LATEST`, `XREF-FIND`, `XREF`,
  `SEE`, `WORDS`) over source search. If the existing words do not expose the
  needed relationship, extend that in-image surface instead of normalizing text
  search as the semantic path.
- **Snapshot checker state must fit DATA, not only mmap:** `USIGS` can grow
  dynamically while checking, but warm snapshot images must persist the table
  into the DATA-resident boot buffer before exec. When warm-image composition
  grows, size the persisted boot capacity for the supported gate workload and
  rerun the native snapshot/full gate.
- **Typed-local checks happen before validation:** splitting or moving Forth
  source can make old bare locals look newly introduced. Run the diff locals
  scan before focused tests, annotate concrete roles (`n`, `len`, etc.), and
  let `bin/hb` reject wrong annotations instead of treating the move as legacy.
- **Diff lint must stream large patches:** `typed-local-diff-lint` read the diff
  dynamically but still fed all lines through the capped shared `SPLIT-LINES`
  table. Commit gates should process patch lines in place and keep a fixture
  above the old 1024-line limit.
- **Keep global warm images lean:** baking large one-off tool cores into the
  shared warm image can leave too little dictionary/data headroom for tools that
  are loaded later. Prefer explicit multi-file warm loads for the boundary that
  needs them, and measure cached child time separately from warm build/cleanup.
- **Do not bake verifier cores into the gate runner for one slice:** adding
  `src/habu/verify-source.f` to `TR-RUNNER-SUPPORT-FILES` overflowed the warm
  runner's persisted checker signature table (`checker: user sigs snapshot too
  large`). Batch dictionary checker cases through the existing checker warm image
  unless the shared runner really needs the verifier resident.
- **GPU readback args are dst-then-src; verify the golden by hand:** maki AXPY on
  the Orin produced `y[0]` unchanged because `cuMemcpyDtoH_v2` was called as
  `(srcDevice dstHost n)` — it is `(dstHost srcDevice n)`. The kernel was correct
  the whole time; only readback failed. A per-call `rc` print localized it (all
  rc=0 except `dtoh=1`). Separately, the test's own golden was wrong: `2*4+40=48`,
  not 44 — the GPU was right, the assertion was the bug. Compute goldens, never eyeball.
- **A retained CUDA primary context hangs at process exit, not in the kernel:**
  the emitted SOFTMAX-ROWS "hung" — but stderr markers (fd 2, unbuffered syscall)
  showed `cuCtxSynchronize` RETURNED; the stall was teardown with a live
  `cuDevicePrimaryCtxRetain` + loaded module. Adding `cuModuleUnload` +
  `cuDevicePrimaryCtxRelease` before `bye` exits clean. Buffered stdout (and any
  `tr`/`tail` in the pipe) is lost on a hang — localize with fd-2 markers to a file.
- **PTX collectives need op-local inactive identity:** `lib/ptx/cg-collective.f`
  now writes `active ? tile : identity(op)` before the shared-memory fold, so
  `BLOCK-MAX` gets -inf and `BLOCK-SUM` gets 0 regardless of how `ROW-LOAD` seeded
  inactive lanes. Direct row-sum/backward paths cannot rely on softmax's accidental
  `EXP(-inf)=0` behavior.
- **Stack-signature tokens must be TYPES, not role words:** `( got expected -- bool )`
  silently binds `got`/`expected` as fresh type vars, so a downstream `n n` op
  mismatches with a confusing `at '<='`. Use `( n n -- bool )`. Locals `{: got :}`
  may use any name; the `( ... )` stack sig may not.
- **lib/ subdirs are research sub-libraries, not flat stdlib:** the stdlib manifest
  grammar (`SMT-LIB-FILE?`) only admits flat `lib/<module>.f`, but the coverage
  walk recursed and demanded module rows for `lib/ptx/*.f` it could not express.
  Gate `SMT-COLLECT-LIB-FILE` on `SMT-LIB-FILE?` so coverage tracks only flat
  modules; nested dirs (`lib/ptx/`) stay trust-audited + `-test.f` + gate-covered
  but out of the curated public-API manifest (mirrors top-level `maki/`).
- **Warm-image tests must use the warm form, not re-`--load` bundled libs:**
  `repair-packet-test`'s no-args case ran `$HABU_WARM_TOOLS --load lib/... repair-packet.f`
  — recompiling libs the warm binary already has (~3s uncontended vs ~0.5s warm
  form). Under ~14-way pool concurrency it blew past the 10s timeout, so the
  subprocess came back `TIMEOUT` not `EXIT`: the tell was assertions on `kind`/
  `code`/stderr failing while the `stdout==0` assertion passed. Route every warm
  subprocess through `WR-TOOLS-LOAD`.
- **DATA layout overlaps can masquerade as checker hangs:** package runtime cells
  at `$268..$280` overlapped the JIT virtual-stack value array (`VVAL-OFF=$250`);
  a fifth literal `10` wrote `$270`, poisoned `PKG-PRI-CELL`, and sent lookup into
  bogus wordlist state. Relocate persistent cells outside JIT scratch ranges and
  gate with a five-literal compiler regression.
- **Device-FFI "launch hang" RCA: write markers to a FILE, not piped stdout.** A
  device gradcheck (`tools/ptx/gradcheck.f`) looked like it hung mid-launch, but
  piped stdout block-buffers so `type cr` progress markers were lost on the
  timeout-kill. File-output markers showed every cuda call returned — the hang was
  at process EXIT from a retained primary context never released (the line-169
  gotcha). Always pair `cuDevicePrimaryCtxRetain` with `cuModuleUnload` +
  `cuDevicePrimaryCtxRelease` before exit (a `GC-FINI`); `acc-device-test.f` works
  only because it does.
- **All-errors checks should be scoped, not subprocessed:** per-definition
  `bin/hb --load ...` gave correct stderr isolation but dominated the tool gate.
  Use a checker registry scope plus diagnostic-buffer capture for normal
  duplicate-definition semantics, and reserve candidate scope only for generated
  candidate checks where shadowing loaded libraries is intentional.
- **Tool fixtures need entry/lib splits:** if a `*-test.f` validates semantic JSON
  or text output by spawning `bin/hb --load ... tool.f`, it rebuilds the same core
  for every case. Put reusable logic and buffered output in `*-core.f`, leave
  `tool.f` as a thin `MAIN` entry, and keep only one CLI smoke for argv/wrapper
  behavior.
- **Evaluate frames must live outside compiler scratch:** `EVAL-FRAME=$280`
  overlapped `VVAL-OFF=$250` and `SNAPSTK-OFF=$360`; compiling a seven-argument
  call inside `evaluate` overwrote the saved outer `INP` and crashed on the next
  token. Keep re-entrant evaluator frames above scratch and below `DATA-START`,
  with an engine-suite high-arity `evaluate` regression.
- **Create persistent gate roots before content-key hashing:** per-file digest
  cache uses the gate warm root's `content-key.cache`, so the gate must create
  the persistent root before any `CK-FILE+` key construction, including
  under-test cache restore.
- **Build-fixpoint child helpers own argv state:** warm gate images reuse one
  process across phases, so stale `PROC-ARGV` entries from a prior tool command
  can leak into `BF-RUN-LOAD-STAGE` and run the wrong child entry. Every `BF-*`
  spawn helper resets argv before preparing its own stage/exe command.
- **Keep FFI ABI separate from loader binding:** AAPCS64 marshalling, out-params,
  and `void** kernelParams` are portable and gateable on every host; `DLOPEN`/
  `DLSYM` depend on OS image support (`DLOPEN-SLOT`/`DLSYM-SLOT` on dynamic ELF).
  Put portable helpers in `lib/ffi-abi.f`, keep `lib/ffi.f` as the loader layer,
  and invoke gate slices through their wrapper so includes like `content-key.f`
  are owned in one place.
- **Spawn command builders should return a checked summary:** a helper that mutates
  global `PROC-ARGV`/`PROC-ENV` and then immediately spawns can fail invisibly if
  the caller never observes the prepared state. Return the prepared argc (or an
  equivalent checked summary) and assert it before `PROC-ARGV-PREPARE`; the
  build-fixpoint fixture caught an empty child invocation this way.
- **Gate timing RCA separates cache-fill from hot architecture:** after a native
  spawn/harness rebuild, the full gate measured 65.093s internal with warm
  misses/candidate install before the early-lint scheduler fix, 41.551s internal
  with one warm refill after the fix, then 30.123s internal hot with warm-miss=0
  and candidate-hit=1. Treat cache-fill as budget coverage and hot-cache as the
  harness architecture number; do not compare cache-fill to prior hot runs.
- **Early lint phases still need warm-build quiescence:** `lint-manifest` and
  `lint-libs` need only the under-test binary semantically, but starting their
  nested stdlib pools while tools/check warm images are rebuilding exposed
  intermittent `E-FS-IO` under full-DAG contention. Start them early only when
  `TR-WARM-DONE?` is already true; otherwise let the late DAG overlap them with
  the long checker/tool tails.
- **Spawn boundaries preserve errno until attribution:** Darwin `posix_spawn`
  failure reports carry-set with `x0=errno`; using `SP` in that path produced
  giant bogus negative pids, and collapsing to the wrapper throw left only
  `rc 60`. Success writes a 32-bit `pid_t`, so load it with `LDRW`. Keep raw
  spawns returning `pid|-errno`, prove missing executable as `-ENOENT`, and
  print test/path/errno before resetting argv/env state.
- **Small-engine tools own their non-baked layout deps:** after shrinking
  `bin/hb`, tools like `imgdump` cannot assume target executable constants such
  as `DATA-SIZE` are resident. Load target layout on demand, but keep common
  dictionary layout in the cold prefix so duplicate-definition checks stay
  fail-closed.
- **Trust drift is audit maintenance, not architecture work:** moving emitter
  helpers shifts `TRUSTED.md` site pins and correctly fails `trust-lint`; update
  those rows after deciding the boundary still belongs. Do not hide a red
  `trust-lint` by starting a broad trust-reduction refactor in the same change.
- **Prefer hex for machine-adjacent literals:** byte values, ASCII, masks,
  offsets, format fields, and crypto constants should use `$hex`. Decimal is for
  small counts and human quantities; stage/bootstrap code should not transcribe
  spec constants into decimal just because both parse.
- **Emitter labels must be scoped across nested helpers:** Linux `spawn-io`
  generated a `b .` self-loop because `LINUX-SPAWN` stored branch labels in
  shared `LNX-*` variables, then nested helpers reused `LNX-DONE`. Use typed
  label locals for emitter control flow whenever a word calls another emitter
  helper that can allocate labels.
- **Timing budgets are host profiles, not global truth:** macOS ARM64, generic
  Linux ARM64, and Jetson/Orin have different CPU envelopes even with the same
  4/2 pool policy. Let the suite auto-detect a concrete host profile, use
  `--cold-cache` for cache-fill proofs, and reserve generic budgets for local
  safety checks.
- **In-process fixtures must restore global harness state:** `gate-stats-test`
  set `GS-ROOT!` to a temporary file and deleted it; the next in-process test
  then failed recording `GS-SPAN`. Any fixture that mutates shared harness roots,
  argv/env, output buffers, or cleanup registries must save/restore them before
  returning to a resident runner.
- **Pointer slots deserve typed definers:** repeated `variable FOO` plus
  `FOO 0 ptr-field` wrappers create avoidable trust rows and noisy manifest
  drift. Use `PTR-VARIABLE` for pointer-valued global cells and `PTR-FIELD:`
  for pointer-valued structure fields; keep `TRUST` only for real pointer
  refinement boundaries such as raw `mmap` results or byte-offset arithmetic.
- **Structured variable records must add cursor first:** when replacing
  `cell+ len + UALIGN ...` arena math with structure sizes, compute
  `cursor HEADER + len + UALIGN ...`; `cursor HEADER len + ...` leaves the
  cursor underneath the computed size and can fail as an opaque source-load
  throw. Preserve the tail base while writing later fields such as `*.SYM`.
- **Do not reuse VM layout names for checker structures:** names like `CF-REC`
  may already be target layout constants. Structure size words are real
  dictionary entries, so use distinct checker-local names such as `CFS-REC`
  instead of relying on silent shadowing.
- **Checker delete rows need a nonzero first cell:** append-only delete/clear
  rows can carry flag `0`, so fixed DFER/NORET records put `SYM` first and use
  only a zero symbol cell as the table terminator.
- **Internal checker effects should not parse strings:** literals, cell
  fetch/store, and control words are checker-owned semantics. Build their rows
  directly and reserve signature parsing for source comments and audited
  `TRUST` input.
- **PTX extent variables avoid reserved one-letter types:** in signatures, `r`
  is float and `c` is char, not type variables. Use tokens such as `e`/`k` for
  matrix row/column extents; otherwise diagnostics report concrete `r`/`c`
  mismatches instead of the intended parametric shape.
- **PTX optimizer values should be structure records:** a value-numbered IR fits
  checked Habu as explicit node records plus construction-time canonicalization:
  fold constants and peepholes before interning, use commutative key ordering for
  CSE, and run DCE as a root live-mark pass with static value fixtures before any
  device/lowering claim.
- **AD straight-line boundaries need named errors:** reverse-mode source reversal
  must reject control-flow tokens case-insensitively before VJP lookup. Letting
  `if`/`loop` fall into a generic no-VJP path hides the real unsupported
  capability and gives weaker repair packets.
- **LOAD adjoints default to scatter-add:** without a checked read-once witness,
  the AD pass must accumulate cotangents with `SCATTER-ADD`/`ROW-SCATTER-ADD`.
  Plain store is an optimization gated by an affine/read-once proof, not an
  inference from the current per-thread effect system.
- **Read-once is a distinct PTX address-space token:** model the optimization as
  `space-global-once` plus `LOAD-ONCE`/`STORE-ONCE`, never as a cast from ordinary
  `span<space-global,...>`. The checker can then reject both accidental plain
  stores from normal spans and accidental use of once spans through normal stores.
- **PTX IR needs distinct symbolic inputs before AD algebra:** a single generic
  `INPUT` node is enough for peepholes, but softmax backward fixtures need stable
  input symbols (`y`, `dy`) plus block-algebra nodes (`BLOCK-SUM`, `B-`) so the
  closed form can be proven as a value graph instead of an output string.
- **Subtree status docs need lint fences, not wording games:** `maki/STATUS.md`
  should be able to own maki counts without avoiding count-shaped prose. Keep root
  self-check counts fenced to root `STATUS.md`, and explicitly skip extracted
  application subtrees in `stale-status-lint`.
- **Concatenated source must preserve `required` identity:** any tool that
  materializes multiple files into one source stream must emit `provided`
  markers before each file. Otherwise dependencies owned by each test/module
  reload already concatenated files and fail with duplicate definitions.
