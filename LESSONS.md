# Lessons

Concise findings only: what worked, what failed, and why. Coding standards live
in `docs/forth.md`; API details live in `docs/` near their feature.

## Current Boundaries

- **One public binary:** `bin/hb` is the public interface. It starts a tty REPL,
  reads piped stdin, runs `hb script.f args...`, or runs a source list as
  `hb --load lib.f tool.f -- args...`. Build-only engines stay temporary under
  `HB_TMP`; do not publish legacy aliases or maker binaries.
- **Background Codex agents need explicit stdin/output:** when launching
  `codex exec` in the background, redirect stdin from `/dev/null` and pass
  `--output-last-message /tmp/name.out`; stdout logs alone can be empty or noisy
  and are not reliable evidence of the scout result.
- **No-binary recovery is native-seeded:** `bin/hb` is generated/ignored. Recover
  with `tools/seed.sh /path/to/hb-seed`; optional SHA-256 verification plus the
  immediate native build-fixpoint install make the installed binary current-source
  native, without making gforth the normal trust root.
- **No hosted bootstrap in daily work:** daily work uses `bin/hb`, the native
  build-fixpoint installer, and `test/run.f`. No-binary recovery uses `tools/seed.sh`.
- **Early stdlib fixtures can load libraries directly:** before manifest/bundle
  plumbing exists, focused `tools/*-test.f` fixtures can concatenate `lib/*.f`
  directly with their driver to test canonical library policy in isolation.
- **Native rebuilds need private temp dirs:** parallel jj workspaces share host
  `/tmp`; fixed names like `stage2-got` race. Native build flows,
  `tools/seed.sh`, and `test/run.f` allocate and export private `HB_TMP`
  by default where they run builds, while preserving explicit `HB_TMP` for repro.
- **Build temp overrides must drive every path:** if a checked build helper
  accepts a temp override, both parent artifact paths and child `HB_TMP` must
  derive from the same getter. Splitting those paths writes sources in one temp
  tree while the spawned maker reads another.
- **Build CLIs own temp override setup:** if `HB_TMP` names an absent directory,
  create it before spawning makers. Letting the maker discover a missing temp
  root later can collapse into an empty nonzero failure.
- **Temp cleanup must not fail the gate:** `trash` can reject private temp
  directories under sandboxed `/var/folders`; cleanup hooks should prefer trash
  but fall back to direct removal when trash itself fails.
- **User-facing builds must fail closed:** `hb-build` verifies user source with
  `CHECK!` and accepts only `-1` certification. Tests assert bad programs fail
  to build, not merely that a later call exits nonzero.

## Checker Soundness

- **Checker RCA starts with the missing invariant:** for any “why didn’t the
  checker catch this?” failure, write `Static invariant:` first, naming what
  should have rejected the program and where it belongs. Runtime fixes wait until
  fail-closed proof, miss class, minimal checked reproducer, and the
  checker/compiler fix or capability dot are recorded; the bad program must end
  as a negative checker regression.
- **Checker misses need class labels:** after fail-closed proof, classify the
  miss as wrong boundary effect, checker semantics, codegen/runtime mismatch, or
  same-type semantic-role gap before editing downstream runtime code.
- **Checker RCA audits shadows before semantics:** after proving the path is
  checked, confirm recently loaded words did not shadow built-ins. `CR constant`
  shadows `cr` in Habu's case-insensitive dictionary, so a later `cr` pushes a
  cell and the checker correctly reports the next consumer.
- **Checked `catch` is stack-preserving quotation catch:** when a negative test
  catches a word whose success path has outputs, consume those outputs inside the
  quotation (for example `[: WORD drop ;] catch`). Do not widen the checker model
  to arbitrary execution-token catch.
- **Stack comments use types, not role names:** checked effects must say
  `( n n -- )`, `( bool -- )`, or `( ptr u8 n -- )`; role names like
  `( got want -- )` are not type declarations and will fail at checked call
  sites.
- **`CHECK!` is the user contract:** dogfood inference (`CHECK`) proves internal
  consistency; user-facing checked builds must verify the body against the
  declared `( in -- out )` and make rejection fatal.
- **Bool paths must all return bool:** early exits in typed predicates need a
  real false value (`0 0= 0=` or a helper), not raw `0`; otherwise path merging
  correctly rejects the definition.
- **Bool arguments must be bools, not sentinels:** checked callees declared with
  `bool` reject raw `0`/`-1` flag conventions. Use a typed bool producer such as
  `STR-FALSE`/`STR-TRUE` or a local module helper at every call site.
- **Record declared signatures after verification:** successful `CHECK!` stores
  the raw declared signature string. Rendering mutated inferred terms can corrupt
  quotient/combinator schemes and break later call-site checks.
- **Recursive calls need fresh signature instances:** in verify mode, `RECURSE`
  applies a fresh copy of the current word's declared effect. Reusing the parsed
  declaration aliases rows and false-rejects recursive definitions.
- **Bool and malformed-signature false-certs are first-class regressions:**
  `( i64 -- i64 ) 0=` and malformed `( i64 )` bodies must reject, with diagnostics
  preserving concrete names such as `bool` vs `i64`.
- **`LEAVE`/`EXIT` typing is path-sensitive:** loop exits constrain the loop-exit
  row, and `EXIT` folds early-return states into the declared output. Divergent
  path arities are soundness bugs, not precision tradeoffs.
- **Execution is the property-test oracle:** `test/prop-test.f` generates typed
  defs, checks them, runs certified ones in-process via `evaluate`, and fails
  only on `certified AND measured != declared`. Generator bugs should become
  rejections, not false-cert reports.
- **False-rejects must be confirmed by execution:** count incompleteness only
  after compiling an unchecked copy and measuring that it really matches the
  declared effect.

## Runtime And REPL

- **Baked REPL support needs an explicit hook boundary:** installed `hb` preserves
  its check hook. Baked REPL/stepper/debug source is trusted engine UI code, so
  the snapshot prepends `0 set-check` and then reinstalls a `CHECK!` hook before
  user input begins.
- **`evaluate` is re-entrant state, not a normal call:** `B-EVAL` saves INP/INE,
  SP, XDS, CP, NDICT, DP, and the return address, then branches to the interpreter
  top. Clean end keeps definitions; error restores compile state and sets
  `EVALERR`.
- **Fixed DATA cells require layout audits:** placing `EVAL-FRAME` in a
  free-looking gap overlapped register allocator tables and produced invalid
  ARM64 fields. Centralize fixed header offsets and verify non-overlap.
- **Large buffers belong outside DATA:** loading `source-lex`, `forth-task-lines`,
  and `attempt-solutions` exposed DATA pressure, but the long-term rule is not
  "pick a bigger fixed ceiling." Static dictionary storage can use DATA; runtime
  source/report/JSONL buffers use the typed OS-backed memory API so any needed
  count of 64K buffers is limited by the OS, not by `DATA-SIZE`. Size runtime
  spans from measured byte needs, round through `MEM-ALLOC-64K-SPAN`, and keep a
  composed scanner regression so the real libraries, not just anonymous mappings,
  prove `here` stays unchanged.
- **DATA-base literals break self-hosted moves:** a first-generation stage can
  still run old startup code while interpreting new source. Source-level helpers
  such as env/REPL must derive the live base through `data-base`, not a duplicated
  numeric DATA address.
- **Capacity probes must be real source files:** `--load` deliberately leaves
  stdin for tool data, so piping a post-load probe to fd 0 does not execute it.
  Put the probe in an explicit loaded source file when measuring `here`/metadata
  after a bundle.
- **Gate load lists need factoring, not long physical lines:** definitions-only
  loading of `test/gate-stdlib.f` exposed `GS-ARG+` at top level when repeated
  LLM driver load lists approached the reader buffer. Factor shared load groups
  into helper words and keep source lines short.
- **Stdin fixture newlines need explicit bytes:** Habu `s" ...\n"` keeps the
  backslash and `n` bytes. Process smoke tests that need a newline should use a
  byte buffer with `10 c,` or append `STR-LF`, otherwise the child may parse a
  word such as `cr\n`.
- **Checker buffers must scale with composed tools:** `tools/check.f` reads the
  concatenated source into `CHK-SRC-BUF`; when real checked tool bundles exceed
  that cap, raise the runner buffers instead of splitting the source to dodge the
  check path.
- **Pipe mode and script mode are distinct:** non-tty stdin with bytes is pipeline
  mode even when `argc > 1` (needed for `bin/hb seed count < test/prop-test.f`).
  Empty non-tty stdin with `argc > 1` runs `argv[1]` as a script path.
- **`--load` leaves stdin as tool data:** `bin/hb --load source... -- args...`
  is explicit source-list mode. Startup reads the listed files and does not
  consume fd 0, so checked tools can use `READ-STDIN-ALL` for data stdin.
  Plain `bin/hb args... < program.f` remains pipeline source mode for
  property/benchmark seed arguments.
- **PTY behavior needs a real pty harness:** `script(1)` interleaves echo/output.
  Drive a pty directly and poll for exit when testing prompt, raw mode, history,
  Ctrl-C/Ctrl-D, and async termination.

## Native Codegen And AOT

- **Parity is native fixpoint, not mirror drift:** the active parity proof is
  byte-for-byte self-rebuild through the build-fixpoint installer. Do not keep bootstrap
  token-diff lints alive as a second source of truth.
- **AOT compaction requires an old-to-new map:** removing call-stencil padding is
  safe only when every PC-relative source (`B/BL`, conditional branches, `ADR`)
  is remapped and range-checked. Keep mapper cursors separate from copy cursors.
- **AOT capability checks need source and closure passes:** inlined data-space
  primitives such as `here` can disappear from call-graph closure scans. Reject
  unsupported words with a source-level pass before the maker runs.
- **Snapshot images relocate only engine-text references:** fixed VA regions keep
  dictionary/data addresses valid. Rebase engine-text call chains and seed-prim
  slots; restore live per-boot cells such as argv/envp, RBASE, S0, wordlist, and
  hook cells after copying snapshot data.
- **Path buffers fail closed:** snapshot/build output paths must use guarded
  `PATH0`-style buffers. A too-small ad hoc buffer can overwrite trailer magic
  and silently emit a cold image.
- **AOT closure stress protects old overflow bugs:** when feature growth hits
  dictionary guards, move the guard/capacity; do not shrink the >256-word closure
  fixture below the old failure threshold.
- **Primitive registry growth needs explicit guards:** adding process primitives
  overflowed the old 96-entry seed registry and corrupted the generated stage
  image. Keep registry/name-pool capacities named and checked in `REG-PRIM` so
  growth fails closed at build time.
- **New `TRUST` sites need manifest rows immediately:** raw emitter helpers must
  update `TRUSTED.md` in the same change with the declared effect, reason, and
  tests. Trust-lint will fail otherwise, and the omission hides the real trusted
  base delta.
- **Trust audit dates follow the gate day:** `trust-lint` defaults to
  `epoch-seconds / DATE-SECONDS-DAY`, an epoch/UTC day. Near local midnight, a
  local calendar date can be future to the gate; use an explicit test date or an
  audit date no later than the gate's default day.
- **Seed primitive names can exceed inline storage:** `EMIT-DICT` must encode
  names longer than `DNAME-INL` out-of-line and relocate them during startup.
  Inline-only emission corrupts fixed `DREC` records and later primitive lookup.
- **Label locals must match label values exactly:** an extra local name consumes
  stale generator stack state and can crash much later in unrelated emitted code.
- **Checked process code uses modeled primitives:** `run-rc` executes but is not
  checker-modeled. Use `spawn-io wait-rc` in checked examples until `run-rc` is
  expressed as a checked wrapper or given an audited checker model.
- **Process buffers stay typed only on the stack:** storing caller byte-buffer
  pointers in ordinary variables makes the checker see them as `n` on reload.
  Keep capture buffers as typed locals/arguments and store only numeric fd/count
  state in process variables.
- **Scratch cells are caller state:** helper words must not reuse a caller's
  loop-index scratch variable. `COMMENT-EXPORTS` once handed only newline bytes
  to its line appender because `SOURCE-LINE-END` clobbered the outer `SOURCE-I`;
  inner scans need their own scratch cells or locals.
- **Branch paths must preserve loop indexes:** a successful branch in
  `SMT-CHECK-LIB-COVERAGE` used `drop 2drop` and removed the loop index, so the
  next `1+` ran on an empty stack after the checks passed. Factor row predicates
  or audit both branch effects when a checked tool repeats one row or crashes
  after success.
- **Locals remove inputs from the data stack:** after `{: a b :}`, helper calls
  must pass `a b` explicitly; do not assume the original inputs are still on the
  stack. For checked counted loops, prefer an explicit scratch index when the
  body calls helpers so the call effect stays visible.
- **Locals are input-only:** do not introduce `{:` groups mid-definition to name
  intermediate values. Store intermediate values in owned scratch cells or factor
  a helper whose inputs can be bound at entry.
- **Do not bind locals after `exit`:** the compiler keeps the exit accumulator
  active until `;`, so a later `{:` in that definition aborts as an illegal
  locals group. Keep post-validation values on the stack or bind before the
  early-exit path.
- **Return typed booleans from `bool` words:** raw `0` is an integer cell, not a
  checked `bool`. Use `0 0= 0=` for false and `0 0=` for true in checked
  definitions whose declared effect returns `bool`.
- **Do not compare bools with numeric `=`:** checked `=` is `n n -- bool`, not
  boolean equivalence. Assert the expected branch directly (`TTRUE`/`TFALSE`) or
  write a typed bool helper.
- **Stress fixture source must be legal Forth:** control words such as `?do` and
  `begin` belong inside a definition. A top-level invalid control word tests the
  parser path, not the child timeout/truncation path you meant to exercise.
- **Repeatable fixtures need idempotent setup:** when a focused test may prepare
  the same directory more than once, use `MAKE-DIRS` or clean the tree before
  `MAKE-DIR`; do not let setup order decide whether the test throws `E-FS-IO`.
- **Replay fixtures need immutable candidate paths:** stability checks rerun the
  recorded first-bad path. Tests must not overwrite that file with a later
  repaired candidate or the diagnostic replay result is correctly false.
- **Name numeric buffer-room inputs:** helper effects like `( cap used add -- )`
  are all `n`; a stray `swap` can type-check and still invert capacity/add at
  runtime. Bind `add` at the caller and pass `cap used add` explicitly.
- **Emitter helpers own their output buffer:** a numeric renderer from another
  snippet emitter can type-check while appending into the wrong global buffer.
  Keep byte/number appenders local to the output buffer they mutate.
- **Boundary scans parse Forth tokens:** trusted-boundary guards should scan
  whitespace-delimited Forth tokens and skip comments/string literals. Substring
  scans false-reject names such as `ENTRUSTED-VALUE` and prose comments.
- **Static diagnostic scans need OS-backed buffers:** `tools/check` can certify
  composed source bundles well over 64K. Checker/linter phases must size both
  input source and generated child-program storage from `FILE-SIZE` and
  `MEM-ALLOC-64K-SPAN`; fixed 64K buffers turn valid bundles into silent
  capacity exits before diagnostics run.

## Darwin And Syscalls

- **Raw Darwin syscalls are not libc APIs:** `posix_spawn` syscall 244 takes the
  private five-arg kernel ABI `pid*, path, adesc, argv, envp`; `wait4` status
  needs `(status >> 8) & 0xff`. Check carry and errno-in-x0.
- **Darwin syscall args must be fully initialized:** `gettimeofday` can return
  `EFAULT` when stale positive x2 survives from a prior `write`; set all
  expected zero/null argument registers before `svc`, not just the apparent
  libc parameters.
- **Syscall output buffers need owned storage:** `gettimeofday` writes a 16-byte
  timeval; keep it in an audited DATA header scratch range, not live machine
  `sp`, and test data/return-stack preservation around the primitive.
- **Recursive directory walks need per-depth buffers:** `getdirentries64` records
  are batch-local; recursing with one shared dirent buffer corrupts the parent
  iteration. Keep directory buffers, offsets, record lengths, base cookies, and
  fds indexed by traversal depth; even a global current-record pointer is unsafe
  because the child walk overwrites it before the parent advances.
- **Process redirection uses XNU spawn descriptors:** empty file-action blobs are
  invalid; pass null descriptors when no fd remapping is requested. Mark
  parent-only pipe/pty fds close-on-exec before spawning or the child can inherit
  a write end and never observe EOF. PTY support uses `/dev/ptmx` plus ioctl
  flow, not `forkpty`/`openpty` libc symbols.
- **Darwin benchmark time is not a syscall:** `clock_gettime`,
  `clock_gettime_nsec_np`, and `mach_absolute_time` are libSystem/commpage APIs.
  The no-libSystem monotonic clock reads `CNTVCT_EL0`/`CNTFRQ_EL0` and converts
  with quotient/remainder to avoid `ticks * 1e9` overflow.
- **Path syscall tests need stable path storage:** low-level primitive smoke tests
  should use fixed NUL strings or production path builders. Private checked
  path-copy helpers can make a syscall test fail with `EFAULT` while obscuring
  whether the primitive ABI or fixture glue is wrong.
- **Symlink deletion checks must lstat first:** `EXISTS?`, `FILE?`, and `DIR?`
  follow symlinks, and broken symlinks can look absent. Destructive cleanup and
  `REMOVE-TREE` must test `SYMLINK?` before existence/type checks so they unlink
  the link instead of traversing or ignoring the target.
- **The JIT inliner must reject PC-relative branches:** byte-copying a small
  primitive body with an internal branch preserves the old branch target. This
  made compiled `epoch-seconds` branch back into the seed primitive and return
  `0`; branch-bearing bodies must compile as calls unless relocated.
- **LC_MAIN gets argc/argv/envp in x0-x2:** the kernel start-stack layout is not
  present. Capture x0-x2 at entry and restore live values after snapshot boot.

## Diagnostics And Tooling

- **LLM stdlib examples should stay tied to source-backed rows:** use
  `lib/std.manifest`, `docs/stdlib.md`, and runnable `examples/` fixtures as the
  source of truth so cookbook snippets cite checked signatures and existing
  gates instead of inventing API contracts.
- **Keep fixture DSLs dependency-light:** checked source/TSV builders are useful,
  but adding an extra helper file to already-large live-driver bundles can push
  them over native DATA capacity. Use the helper where it removes local syntax
  hacks, and split heavy bundles before sharing it more widely.
- **Dot hierarchy parents need root files:** add plan children with
  `dot add ... -P <root-id>` only when `.dots/<root-id>/<root-id>.md` exists.
  Do not use `-P` with a nested child id; it creates stray top-level directories
  without root files and breaks `dot tree`. Put second-level dependencies in the
  description instead.
- **Quote dot descriptions defensively:** dot descriptions often contain
  backticks, `$`, or Forth punctuation. Pass `-d` text in single quotes or a
  file-backed command path so the shell cannot execute or expand repro snippets.
- **Diagnostics are an API:** JSON errors carry `schema_version: 1`, source spans,
  verdict, word, token, expected, and actual. Wrappers preserve valid JSON object
  lines and fail nonzero on checker rejection.
- **JSON fixtures need builders:** Habu `s"` does not escape embedded JSON quotes.
  Build expected rows with `lib/json-write.f` or a checked fixture DSL instead
  of writing giant escaped string literals that the interpreter will parse as
  source.
- **Benchmark manifests are multi-harness:** `bench/llm/tasks.tsv` is canonical
  for both reference and live runs; reference metrics must filter
  `harness=forth` instead of assuming every manifest row has a checked answer key.
- **Harness=forth task bodies are emitted in Habu:** use
  `bench/llm/forth-task-lines.f` for headerless `harness=forth` TSV rows instead
  of duplicating `awk`/shell scans in benchmark drivers.
- **Manifest buffers must size for V2 rows:** extra live-run fields can push
  `bench/llm/tasks.tsv` past old compact-reader buffers; keep native validator
  task buffers sized from the expanded manifest, not the old five-column file.
- **Source origins are wrapper-owned:** the checker reports definition-relative
  spans. Build/check wrappers inject origin markers before definitions and keep
  those markers out of final user bundles.
- **Effect drift checks must compare full normalized tokens:** returning on the
  first shared whitespace made `n --` equal `n -- n`. Tokenize/advance through
  the whole effect when comparing manifest text.
- **Public signature rows need effects before locals:** `tools/public-signatures.f`
  only records `: WORD ( in -- out ) ...` forms. Put `{: :}` locals after the
  stack-effect comment in published library definitions so manifest drift checks
  see them.
- **Library helper definitions need manifest rows:** checked `:` definitions in
  `lib/*.f` are captured by public-signature drift checks even when intended as
  internals. Track them as `active` manifest rows instead of leaving drift.
- **Large native tool bundles can corrupt reads:** combining large lint tables,
  `json.f`, and another large file buffer crashed JSON gate assertions. A lean
  standalone reader plus distinct helper scratch variables fixed it.
- **Escaped fixture literals hide shape bugs:** JSON-row tests became clearer
  only after the row shape moved into checked field words with explicit quote
  bytes.
- **Keep fixture DSLs at their sharing boundary:** common live-driver helper
  bundles are capacity-sensitive. Task-specific byte/source builders belong in
  the focused test or driver that owns them until multiple users prove they are
  shared library surface.
- **Captured gate children need heartbeat polls:** a silent child makes the
  parent look hung if `poll` waits for the full timeout. Gate-owned capture loops
  should poll in heartbeat-sized slices and print label-only wait lines while
  keeping child stdout/stderr captured for failures.
- **Generated-source strings need byte fixtures:** `s"` cannot encode an
  embedded quote. When a candidate/test source needs arbitrary bytes, emit a
  byte-backed string word with the checked fixture DSL instead of inventing
  ad hoc escaping.
- **Generated JSON needles need field builders:** backslash does not escape a
  quote inside `s"`. Build JSON substrings with checked byte/field helpers so
  keys and quote bytes are explicit.
- **Generated checked fixtures require immediate RCA:** a fixture that stalls,
  times out, or exits without diagnostics is a checker/harness bug until proven
  otherwise. Isolate the exact phase and root cause before shrinking, bypassing,
  or calling the fixture too large.
- **Native crashes need debugger state first:** for SIGSEGV, DATA corruption, or
  stack corruption, set breakpoints/step and inspect data-stack plus watch cells.
  If the existing debugger cannot expose the state, extend it before resorting to
  print-marker probes.
- **Runtime emitters must preserve the DATA register:** x20 is the engine DATA
  base, not scratch. Any trap/debug/runtime helper that uses `DATA ...` addressing
  must not reuse register 20 for counters or temporaries before the next DATA
  access.
- **Locals already consume their inputs:** after `{: a:ptr u :}`, the incoming
  stack cells are gone. Do not `drop` those inputs again on one branch; legacy
  unchecked tools can turn that underflow into a return-to-zero crash.
- **Generated `die` calls need typed strings:** `die` takes `ptr u8 n n`; `0 0`
  is two integers, not an empty string. Generated harnesses should emit a real
  `s" message"` or a byte-backed string word.
- **Generated Forth comments use one backslash:** `\` is the comment word;
  `\\` is a different token and can turn a fixture prelude into an undefined
  word before the intended diagnostic is reached.
- **Source-use guards need token matching:** required-word checks must compare
  exact source tokens, not substrings; `PROP-DEFAULTS` matched
  `PROP-DEFAULTS-OK?` and let a constant fake through to runtime failure.
- **Keep exact-token helpers load-selective:** moving scanner helpers into the
  base live-driver bundle tipped large file/process tests into native capacity
  failure. Keep optional source guards in a separate helper loaded only by
  drivers that need them.
- **Load transitive stdlib dependencies in child bundles:** `lib/build.f` depends
  on `lib/process.f` for `RUN-RC`; omitting it let non-`BUILD-RUN` rows pass while
  artifact rows died with empty child output.
- **Check phases must be silent:** `hb` can emit checker diagnostics while still
  exiting 0 for a loaded file. Live drivers should treat any stdout/stderr from a
  check-only child as rejection; otherwise rejected code can be executed later.
- **Expected-throw fixtures should stay quiet:** row/error reporters should be
  opt-in at the CLI boundary so negative tests can assert throw codes without
  printing user-facing `FAIL` lines.
- **Do not pull unchecked parsers into checked tests:** `tools/json.f` remains an
  unchecked legacy boundary; checked fixtures should assert with typed helpers
  and leave full JSON parsing to validator gates until that parser is typed.
- **Checked helpers must be defined before use:** a forward reference inside a
  checked definition can fail during load and surface later as an unpublished
  word; move the helper after its dependencies or factor dependencies earlier.
- **Native summary tables need tight capacities:** large parallel arrays consume
  DATA quickly. Size aggregate tables from real cardinality such as
  category/model/arm cells, not from row or group maxima.
- **Unchecked core fixtures stay outside source files:** standalone core sources
  such as `src/core/sha256.f` must not disable checking themselves; put
  `0 set-check` in the fixture harness and keep checked wrappers/tests separate.
- **Useful register lint needs contracts:** clobber analysis must model callee
  returns, preserves, no-return exits, routine boundaries, syscall clobbers, and
  LR/SP conventions; raw write-before-BL/read-after-BL rules are mostly false.
- **Stale binaries hide fixes:** never silence the build while debugging the code
  it builds. Remove output artifacts before generate-then-run tests.
- **Pushes reject conflicted ancestors:** a clean worktree and passing gate do
  not clear jj conflict metadata. Before pushing rewritten stacks, check the
  pushed range for `conflict` and resolve the earliest conflicted commit.
- **Dictionary names are strings, not counted bytes:** keep flags above the
  length field and decode through one helper path. Low-bit flags recreated a
  255-byte cap and made AOT/prof/snapshot consumers disagree.
- **Snapshot scanners need structural proof:** magic constants also appear in
  code. Accept a trailer only when `region-len + data-len` ends exactly at the
  trailer offset; otherwise fallback dictionary scans see false snapshots.
- **No-SIGPIPE still needs normalized syscall failure:** Darwin `F_SETNOSIGPIPE`
  prevents parent termination, but closed-pipe `write` reports `EPIPE=32`; raw
  errno can collide with a valid byte count. Normalize carry in syscall prims,
  then validate counts and keep a 32-byte closed-pipe regression.
- **LLM benchmark reports need separate axes:** keep trial pass, task pass@k,
  repair rounds, wall time, and generated-token cost distinct. Output tokens are
  an effort proxy, not direct access to hidden reasoning.
- **Diagnostic usefulness needs an ablation:** replayable benchmark artifacts make
  rows auditable, but only structured-vs-raw-vs-blind feedback arms show whether
  Habu diagnostics actually help LLM-generated code converge.
- **Ablation plumbing is not evidence:** if a benchmark arm exists only behind an
  opt-in environment variable, the default live runner can silently skip the
  proof. Make required ablations default, with opt-out overrides tested.
- **Live benchmark sweeps must resume by identity:** model calls and driver
  repairs can fail before emitting a row. Resume on `(model_id, arm, task_id,
  trial)` so interrupted JSONL fills missing trials without duplicating evidence.
- **Live runners must enforce exact coverage:** a driver can die before row
  emission while the outer sweep continues. Record expected identities during the
  run and fail before report generation if any row is missing.
- **Dogfood benchmark hot paths:** per-call benchmark glue such as model-response
  parsing must be Habu-native. Host parsers hide missing Habu JSON/string/file
  primitives and make the benchmark less credible as an LLM-native proof.
- **Benchmark row artifacts need streaming JSON:** prompt/response/replay fields
  can exceed small writer buffers. Quote artifact bytes with a chunked Habu
  emitter instead of routing row bodies through fixed-capacity JSON builders or
  host JSON encoders.
- **Diagnostic rows must balance repair stats:** schema-v2 rows with
  `diagnostic_count > 0` need `repair_class_stats` whose diagnostic counts sum
  to the row total. Empty stats are valid only for zero-diagnostic rows.
- **Schema-v2 changes must update every fixture:** report fixtures that only
  invoke validators indirectly still build full rows by hand; add new required
  fields there in the same change or the report child exits before assertions.
- **Codex input-token bloat is ambient context:** default `codex exec` loads
  apps/plugins/tool context and project instructions; a smoke prompt fell from
  about 29k input tokens to about 11.5k by using a clean `CODEX_HOME` plus
  disabled apps/plugins/tool features. Benchmark rows still report generated
  output tokens only; raw artifacts preserve full Codex usage for audits.
- **LLM validator fixtures should isolate corpus churn:** generate temporary
  reference metric rows from the task manifest under test instead of assuming the
  checked-in reference JSONL has already been refreshed for concurrent task
  additions.
- **Cross-row JSON state must copy strings:** `JSON-STRING$` points into the
  parser's reusable string buffer. Validators that compare data across
  `JSON-PARSE` calls must copy string bytes before parsing the next row.
- **Doc contract fixtures need stable anchors:** when `grep -F` gates API prose,
  keep the asserted phrase contiguous in Markdown or assert a shorter stable
  substring; ordinary line wrapping can otherwise hide a present contract.
- **CLI emitters validate before writing stdout:** source-list style tools should
  parse and reject bad arguments before emitting prefixes; otherwise usage errors
  can leak partial machine-readable output.
- **Return-stack repair fixtures need data-effect parity:** `fix_return_stack`
  appears only when the data stack already matches the declaration; a bad `>r`
  that also removes a declared data output is correctly classified as
  `add_producer` first.
- **Repair diagnostics need source effects too:** normalized rows such as
  `a -- a` erase user row names like `R x -- R x`; keep a source-preserving
  effect field in diagnostics and repair packets for LLM repair context.
- **Broad unchecked tool ports hide the bugs being chased:** reinstall the
  `CHECK!` hook immediately after raw declarations and lint protected files for
  normal definitions left under `0 set-check`.
- **Debug tool CLIs still obey checker rules:** direct Habu replacements for
  shell probes should load dependencies with `--load`, keep top-level control
  flow inside a word, and expose raw pointer cursors through audited accessors.
- **Native report tools should stream large inputs:** stacking multiple
  256K-512K capture/read buffers can push later data objects into unsafe space.
  Count/scan large JSONL inputs in chunks and reserve fixed capture buffers only
  for bounded summaries.
- **Report reducers need dedicated scratch cells:** loop scratch such as
  `RR-I/J/K` is clobbered by nested stats, sort, and render helpers; persist
  cross-call metrics in purpose-named variables.
- **Checked tool fatal paths use the modeled `die` effect:** `die` is modeled as
  `( ptr u8 n n -- )`; old unchecked `s" msg" type cr 1 die` branches do not
  type-check. Use `s" msg" code die` or a checked wrapper.
- **Shared tool libraries must hand off checker state deliberately:** when a
  bundled helper library re-enables `CHECK!`, end with an explicit boundary if
  downstream legacy scanner modules are expected to choose their own hook.
- **Immediate-word fixtures extend to compiled callers:** a checked definition
  that uses an unchecked immediate word executes that word during compilation.
  Keep wrappers for `postpone`/`compile,` primitive fixtures inside the same
  tested boundary, then restore `CHECK!` afterward.
- **Dynamic pointer tables need typed accessors:** store `mmap` results in raw
  cells, but expose arrays/string pools through small audited accessors with
  `TRUSTED:` effects. Checked callers keep seeing `ptr` values instead of
  untyped numbers reloaded from variables.
- **Trusted-boundary diagnostics must classify every escape hatch:** catching
  `evaluate` is not enough; `TRUST` and `set-check` inside checked definitions
  need the same schema-1 `trusted_boundary_required` rejection so repair loops
  do not learn to silence the checker.
- **Low-level byte decoders need simple shift constants:** using computed shift
  counts such as `BYTE-BITS 7 *` inside checked field readers can destabilize
  later locals-heavy definitions. Name the shifts and keep decoder bodies
  straight-line.
- **Do not type-check CLI mains by accident:** the native `tools/check.f` runner
  executes the final source after checking. For scripts with a top-level `MAIN`,
  use a no-main fixture or safe argv/env inputs; otherwise the check can launch
  the live tool and write default artifacts.
- **Checker fixture token tables must scale dynamically:** focused stdlib
  checker runs concatenate dependencies plus the fixture under `tools/check.f`.
  If a composed load overflows `tools/lint/source-lex.f`, fix the shared token
  metadata storage with memory-backed growth; do not split tools or drop the
  diagnostic check to dodge a static cap.
- **Process capture fixtures must allow fork paths:** tests that capture
  `/bin/pwd` need output buffers sized for isolated jj workspace paths, not just
  the short main checkout path; otherwise valid forked workspaces throw
  `E-PROC-TRUNCATED` before the targeted change is exercised.
- **Large driver tests should use top-level DSL assembly:** benchmark driver
  test bundles can hit raw parser/checker capacity when every long candidate
  snippet is a published helper. Keep reusable vocabularies checked, then build
  long per-case snippets with those words in the top-level test runner.
- **Undefined tokens near a huge word can be BODYBUF pressure:** if adding a
  small string to a large raw definition makes later tokens in that same
  definition appear undefined, factor the colon body into section words before
  blaming source input or code-region size.
- **A lone `:` on large loads is the code-region guard:** confirm with
  `cp@ dbase@ -`; if it is at or past `REGION - $4000`, a colon definition is
  failing closed before it can emit a useful diagnostic. Remove one-use compiled
  fixture helpers first, then grow `REGION` deliberately when live tools outgrow
  the engine.
- **Exit 77 with `:` is the dictionary guard:** if `cp@ dbase@ -` is well below
  `REGION - $4000`, check `ndict@` against `DICT-CAP`. Grow `CFSTK-OFF`,
  `DICT-CAP`, and `DICT-SIZE` together so dictionary slots still end at CFSTK
  and code keeps the guard gap.
- **Checker capacity bumps are layout work:** increasing global checker tables
  can shift native data layout and break unrelated fixtures. Prefer removing
  one-use fixture signatures first; expand engine regions deliberately.
- **Repo edits go through `apply_patch`:** even mechanical replacements and
  long one-line gate updates should use patches so accidental broad rewrites,
  duplicate definitions, shell-expanded capture groups, and rule violations stay
  reviewable.
- **Do not parallelize VCS status commands:** `jj st` and `git status` can race
  on `.git/index.lock`. Run Git/JJ index-touching commands sequentially.
- **Use only documented dot commands:** `dot active` is not a status command and
  can create a malformed dot; subcommands such as `dot purge` do not support
  `--help` and may mutate state. Use `dot add "Title" -d "Full context..."`,
  `dot ready`, `dot ls`, `dot tree`, `dot show <id>`, and
  `dot on <id>`/`dot off <id> -r "completed: evidence..."`.
- **Same-typed string pairs need order tests:** the checker cannot distinguish a
  path `(ptr u8 n)` from stdin bytes `(ptr u8 n)`. When a helper takes multiple
  string pairs, add a focused test or first live use that proves the semantic
  order, not just the stack type.
- **Habu-spawned `hb` children need explicit env inheritance:** shell-launched
  children inherit env automatically; `RUN-ARGV-CAPTURE` does not recreate that
  contract. Gate runners that spawn nested `hb` should use the env-aware process
  APIs plus `PROC-ENV-INHERIT-MISSING`.
- **Undefined convenience words fail before type checking:** `0<>` is not a Habu
  word. Use explicit `0 = 0=` (or add/test a real word) instead of assuming
  common Forth conveniences exist.
- **Do not all-errors check full dependency bundles:** `tools/check.f
  --all-errors` checks top-level definitions independently; on bundled stdlib
  sources it can report dependency constants and helper words as undefined.
  Use dependency-aware `tools/check.f --json-errors` for full driver bundles,
  and reserve all-errors diagnostics for focused candidate/source checks.
- **All-errors is a failure diagnostic, not a valid-bundle preflight:** running
  the per-definition checker before every successful `tools/check.f` source run
  multiplies gate time by definition count. Run the fast fail-closed bundle
  check first; invoke all-errors only after failure or explicit `--all-errors`.
- **Reproduce `GS-CHECK-RUN` with raw stdin source:** the gate feeds bundled
  source bytes to `tools/check.f` over stdin and lets the wrapper add its own
  check prefix. Do not add a manual prefix or switch to file-argument mode when
  isolating a gate check failure; those are different code paths.
- **Keep catch-based parsers behind checker contracts:** `tools/json.f` has a
  deliberate JSONL `catch` recovery boundary. New checked reducers that call it
  should use small checked interface fixtures for checker runs and runtime tests
  with the real parser until the typed `catch` dot is implemented.
- **Progress must exist at every blocking layer:** child phases can print
  heartbeats and still leave the user blind if the parent waits with a raw
  `WAIT-RC`. Top-level gate runners need their own poll loop, timeout, and
  heartbeat while they stream child output.
- **Live-row bundle artifacts must stay small:** `live-row.f` embeds artifact
  contents before hashing. Native drivers that need a large internal run bundle
  should keep that path separate from the emitted `final_bundle` artifact until
  row emission is streaming.
- **Stage2 source cap is a builder contract:** AOT maker generation can exceed
  the old 256 KiB stage2 reader even when the user source is tiny. Reproduce with
  `hb-build` child output and size `stage2-src`; then raise the named stage2
  source cap deliberately while preserving fail-closed overflow behavior.
- **Live-row extras come after `DS-LR-*`:** driver result setters call
  `DS-CONFIG-LR-COMMON`, which resets live-row fields. Set driver-specific
  extensions such as runtime measurements after `DS-LR-PASS`/`DS-LR-FAIL`.
