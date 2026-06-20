# Lessons

Concise findings only: what worked, what failed, and why. Coding standards live
in `docs/forth.md`; API details live in `docs/` near their feature.

## Current Boundaries

- **One public binary:** `bin/hb` is the public interface. It starts a tty REPL,
  reads piped stdin, runs `hb script.f args...`, or runs a source list as
  `hb --load lib.f tool.f -- args...`. Build-only engines stay temporary under
  `HB_TMP`; do not publish legacy aliases or maker binaries.
- **No-binary recovery is native-seeded:** `bin/hb` is generated/ignored. Recover
  with `tools/seed.sh /path/to/hb-seed`; optional SHA-256 verification plus the
  immediate `tools/build.sh` fixpoint make the installed binary current-source
  native, without making gforth the normal trust root.
- **No hosted bootstrap in daily work:** daily work uses `bin/hb`,
  `tools/build.sh`, and `test/run.sh`. No-binary recovery uses `tools/seed.sh`.
- **Early stdlib fixtures can load libraries directly:** before manifest/bundle
  plumbing exists, focused `tools/*-test.f` fixtures can concatenate `lib/*.f`
  directly with their driver to test canonical library policy in isolation.
- **Native rebuilds need private temp dirs:** parallel jj workspaces share host
  `/tmp`; fixed names like `stage2-got` race. `tools/build.sh`,
  `tools/seed.sh`, and `test/run.sh` allocate and export private `HB_TMP`
  by default where they run builds, while preserving explicit `HB_TMP` for repro.
- **Temp cleanup must not fail the gate:** `trash` can reject private temp
  directories under sandboxed `/var/folders`; cleanup hooks should prefer trash
  but fall back to direct removal when trash itself fails.
- **User-facing builds must fail closed:** `hb-build` verifies user source with
  `CHECK!` and accepts only `-1` certification. Tests assert bad programs fail
  to build, not merely that a later call exits nonzero.

## Checker Soundness

- **`CHECK!` is the user contract:** dogfood inference (`CHECK`) proves internal
  consistency; user-facing checked builds must verify the body against the
  declared `( in -- out )` and make rejection fatal.
- **Bool paths must all return bool:** early exits in typed predicates need a
  real false value (`0 0= 0=` or a helper), not raw `0`; otherwise path merging
  correctly rejects the definition.
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
- **Data-space growth must fail closed:** large native tool bundles can push
  interpret-mode `DP` near the mapped DATA limit; allocation paths need explicit
  bounds checks so growth exits with a capacity error instead of faulting on the
  next `s"`/store.
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
  byte-for-byte self-rebuild through `tools/build.sh`. Do not keep bootstrap
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
- **Benchmark manifests are multi-harness:** `bench/llm/tasks.tsv` is canonical
  for both reference and live runs; reference metrics must filter
  `harness=forth` instead of assuming every manifest row has a checked answer key.
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
- **Large native tool bundles can corrupt reads:** combining large lint tables,
  `json.f`, and another large file buffer crashed JSON gate assertions. A lean
  standalone reader plus distinct helper scratch variables fixed it.
- **Fixture structure deserves checked vocabulary:** when tests need structured
  data such as JSON rows, factor a small checked writer/DSL instead of relying on
  giant quoted literals or private byte emitters. This keeps escaping and shape
  bugs in reusable Habu code.
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
- **Generic syscall prims expose raw positive errno:** `open`, `read`, and
  `write` do not normalize carry like `open-rd`/`access`/`stat64`. Checked file
  helpers must use normalized prims where available and validate syscall counts
  instead of treating only negative values as failure.
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
- **Checker fixture bundles need token headroom:** focused stdlib checker runs
  concatenate dependencies plus the fixture under `tools/check.f`. Keep
  `tools/lint/source-lex.f` token capacity sized for real checked bundles rather
  than dropping the diagnostic check when adding a library module pushes past an
  old cap.
- **Process capture fixtures must allow fork paths:** tests that capture
  `/bin/pwd` need output buffers sized for isolated jj workspace paths, not just
  the short main checkout path; otherwise valid forked workspaces throw
  `E-PROC-TRUNCATED` before the targeted change is exercised.
- **Repo edits go through `apply_patch`:** even mechanical replacements should
  use patches so accidental broad rewrites, duplicate definitions, and rule
  violations stay reviewable.
