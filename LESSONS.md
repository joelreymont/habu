# Lessons

Concise findings only: what worked, what failed, and why. Coding standards live
in `docs/forth.md`; API details live in `docs/` near their feature.

## Current Boundaries

- **One public binary:** `bin/hb` is the public interface. It starts a tty REPL,
  reads piped stdin, or runs `hb script.f args...`. Build-only engines stay
  temporary under `HB_TMP`; do not publish legacy aliases or maker binaries.
- **No-binary recovery is native-seeded:** `bin/hb` is generated/ignored. Recover
  with `tools/seed.sh /path/to/hb-seed`; optional SHA-256 verification plus the
  immediate `tools/build.sh` fixpoint make the installed binary current-source
  native, without making gforth the normal trust root.
- **Gforth is historical bootstrap only:** daily work uses `bin/hb`,
  `tools/build.sh`, and `test/run.sh`. No-binary recovery uses `tools/seed.sh`;
  the old gforth oracle script is retired.
- **Native rebuilds need private temp dirs:** parallel jj workspaces share host
  `/tmp`; fixed names like `stage2-got` race. `tools/build.sh`,
  `tools/seed.sh`, and `test/run.sh` allocate and export private `HB_TMP`
  by default where they run builds, while preserving explicit `HB_TMP` for repro.
- **User-facing builds must fail closed:** `hb-build` verifies user source with
  `CHECK!` and accepts only `-1` certification. Tests assert bad programs fail
  to build, not merely that a later call exits nonzero.

## Checker Soundness

- **`CHECK!` is the user contract:** dogfood inference (`CHECK`) proves internal
  consistency; user-facing checked builds must verify the body against the
  declared `( in -- out )` and make rejection fatal.
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
- **Label locals must match label values exactly:** an extra local name consumes
  stale generator stack state and can crash much later in unrelated emitted code.
- **Checked process code uses modeled primitives:** `run-rc` executes but is not
  checker-modeled. Use `spawn-io wait-rc` in checked examples until `run-rc` is
  expressed as a checked wrapper or given an audited checker model.

## Darwin And Syscalls

- **Raw Darwin syscalls are not libc APIs:** `posix_spawn` syscall 244 takes the
  private five-arg kernel ABI `pid*, path, adesc, argv, envp`; `wait4` status
  needs `(status >> 8) & 0xff`. Check carry and errno-in-x0.
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

- **Diagnostics are an API:** JSON errors carry `schema_version: 1`, source spans,
  verdict, word, token, expected, and actual. Wrappers preserve valid JSON object
  lines and fail nonzero on checker rejection.
- **Source origins are wrapper-owned:** the checker reports definition-relative
  spans. Build/check wrappers inject origin markers before definitions and keep
  those markers out of final user bundles.
- **Effect drift checks must compare full normalized tokens:** returning on the
  first shared whitespace made `n --` equal `n -- n`. Tokenize/advance through
  the whole effect when comparing manifest text.
- **Large native tool bundles can corrupt reads:** combining large lint tables,
  `json.f`, and another large file buffer crashed JSON gate assertions. A lean
  standalone reader plus distinct helper scratch variables fixed it.
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

## Historical Bootstrap

- **Old gforth runs need isolation:** if deliberately using historical bootstrap
  code, isolate gforth/libcc caches; concurrent runs can corrupt
  `~/.cache/gforth`.
