# Lessons

Last updated: 2026-06-26

Concise findings only: what worked, what failed, why. Coding standards live in
`docs/forth.md`; API details in `docs/` near their feature. One tight bullet per
lesson — keep the specific word/code/path, cut the prose.

## Checker Soundness

- **Check everything you can — byte-emitters included (STRONG RULE):**
  `tools/check.f --source-list` passes on `src/os/image-bytes.f`, so raw
  byte/layout emitters ARE checkable. Default ALL new code (engine, seam,
  ELF/Mach-O emitters, tooling) to checked typed Habu and verify with
  `--source-list`. The unchecked `0 set-check`/`TRUST` boundary is only the
  audited minimum the checker truly can't express — never a default; "not
  expressible as stack effects" is an overclaim, verify first.
- **Use real types, not reflexive `n`:** a dereferenced cell address is `ptr a`
  (so `@` yields its content, e.g. a GOT slot read for `ffi-call`); a string is
  `ptr u8 n`; `n` only for genuine scalars. Stack comments are type tokens, not
  role names — `( n n -- )`, not `( got want -- )` (role names fail at call sites).
- **Every `TRUST` gets a `TRUSTED.md` row in the same change**, with effect,
  reason, tests. Adding lines above existing `TRUST` sites drifts their rows;
  rerun `--source-list`/`trust-lint` and fix the exact sites before the gate. A
  line-only site fix is not a trust-base expansion.
- **`CHECK!` is the user contract:** dogfood inference (`CHECK`) proves internal
  consistency; user builds verify the body against declared `( in -- out )` and
  make rejection fatal. `hb-build` accepts only `-1`; tests assert bad programs
  fail to build.
- **Large words hide unchecked reasoning:** if review needs reconstructing a long
  word's stack by hand, factor first (named helpers with explicit effects), then
  change behavior, so every intermediate contract is checked. Applies hardest to
  raw compiler/register emitters.
- **Classification tables beat token ladders:** `lib/regex.f` stopped repeating
  `dup`/`over` metacharacter chains once escapable, unsupported, and emitted-token
  bytes became table data behind `RX-BYTE-IN?`/`RX-META-TOKEN`. Pair those tables
  with named state-transition helpers so matcher behavior changes stay visible in
  fixtures instead of hidden inside one branch ladder.
- **Checker-miss RCA starts before output:** scan the prompt for
  checker/compiler-miss wording (incl. quoted/meta). First visible line is
  `Static invariant:` naming the pre-runtime fact and the enforcing boundary; if
  unknown, say so and reduce. Runtime fixes wait for fail-closed proof, miss
  class (wrong boundary effect / checker semantics / codegen-runtime mismatch /
  same-type role gap), minimal checked reproducer, and a checker/compiler/primitive
  fix or capability dot; the bad program ends as a negative regression. The rule
  applies even to prompts about enforcing the rule.
- **Checker RCA audits shadows before semantics:** confirm recently-loaded words
  didn't shadow built-ins (`CR constant` shadows `cr`, so a later `cr` pushes a
  cell and the checker correctly flags the consumer).
- **Persistent checker stores must scale and roll back:** `USIGS` grows for real
  composed bundles and rolls back via `UEND` in define-check-discard loops; fixed
  caps are compiler bugs, not reasons to trim. Same for `tools/check.f` source
  buffers and `tools/lint/source-lex.f` token tables — grow with `lib/memory.f`.
- **Typed booleans:** `bool` words return `0 0=`/`0 0= 0=`, never raw `0`/`-1` or
  sentinels; pass bools via typed producers (`STR-TRUE`/`STR-FALSE`). Don't
  compare bools with numeric `=` (`= : n n -- bool`); assert `TTRUE`/`TFALSE`.
- **Path-sensitive control typing:** `LEAVE`/`EXIT` fold early-return states into
  the declared output; divergent path arities are soundness bugs. `RECURSE`
  applies a fresh copy of the declared effect (reusing the parse aliases rows).
  Store the raw declared signature after `CHECK!` (rendering mutated terms
  corrupts combinator schemes).
- **Quotations are xts, not closures:** `[: ;]` may not read a surrounding local;
  checker and compiler reject local refs while a quotation is open until real
  closures exist.
- **Checked `catch` is stack-preserving quotation catch:** consume the success
  outputs inside the quotation (`[: WORD drop ;] catch`); don't widen to arbitrary
  xt catch. `TRUST`/`set-check` inside a checked def need the same schema-1
  `trusted_boundary_required` rejection as `evaluate` so repair loops can't learn
  to silence the checker.
- **Locals are input-only and pre-control:** `{:` consumes its inputs (pass them
  explicitly afterward; don't re-`drop`); no `{:` mid-definition, inside control
  flow, or after `exit` (the exit accumulator stays live to `;`). Prefer a named
  scratch index in checked counted loops whose body calls helpers.
- **False-rejects need execution proof:** count incompleteness only after running
  an unchecked copy and measuring it matches the declared effect (`test/prop-test.f`
  fails only on `certified AND measured != declared`; generator bugs become
  rejections, not false certs). Malformed/`0=`-on-`i64` bodies must reject with
  concrete `bool` vs `i64` diagnostics.
- **Pointer cells need typed accessors:** use the checked `ptr-field` primitive to
  build `ptr ptr x` addresses (`@`/`!` then preserve nested pointer types) instead
  of `TRUSTED:` reload helpers. Store `mmap`/byte-pointer state behind small
  audited `TRUSTED:` accessors so checked callers keep seeing `ptr`, not reloaded
  `n`. Removing a file-scope `0 set-check` finds real bugs (it exposed a
  trust-lint duplicate-manifest residue).
- **Unchecked emitter stack bugs need typed shape gates:** the Darwin spawn
  descriptor underflow was inside an audited native emitter boundary, not user
  checked code. Fix was to factor raw emitters away from locals (`C-LOCAL-REF`,
  `BYTES,`, `REG-PRIM`/`FPRIM`, Darwin spawn helpers), add typed accessor TRUST
  rows, and gate forbidden source shapes in `tools/build-fixpoint.f` before any
  image is written. Use `depth .` rather than `.s` when chasing build-time
  underflow; `.s` can hide negative depth.
- **Same-cell emitter values need nominal roles:** add checker roles for values
  that share runtime representation but must not compose (`reg`, `label`, `va`,
  `symidx`). A stack comment like `( n n -- )` hides swaps; role tokens plus
  negative `CHECK!` fixtures reject them before raw emitter tests run.
- **Checked encoder helpers consume roles, not raw cells:** typed ARM64 helpers in
  `bootstrap/cg/asm-checked.fs` should accept `reg`/`off` and erase with
  `REG>N`/`OFF>N` only at bit-packing leaves. Public encoders then reject
  register-vs-offset swaps while preserving the same instruction bytes.
- **`ptr-field` retires pointer reload TRUST:** variable-backed pointer cursors
  such as `RPD@`, `PR-A@`, `FP-A@`, `EP@`, `BYP@`, and `BYA@` are plain checked
  helpers when written as `VAR 0 ptr-field @`; do not add TRUST rows for this
  storage pattern.
- **Locals shape is a checker invariant:** `{:` is allowed only on live
  top-level paths, before control/quotation frames open and before a dead `exit`
  path. Mid-control locals are static rejections, not build-preflight-only style
  failures.

## Tool & Infra

- **Gate speed RCA follows the phase wall clock:** warm images cut repeated
  inner-tool recompiles; the bigger wall cut came from a bounded checked DAG
  pool. Do not mutate `bin/hb` inside the gate: build candidates under private
  `HB_TMP`, run independent stdlib/diagnostic/engine slices concurrently, bound
  nested pools with `HABU_GATE_POOL_SLOTS`, and delay short timeout-sensitive
  lints until the heavy wave drains.
- **Pool slot state is an invariant:** free=`-1`, active=`0`, done=`1`; set
  active before spawning. A live-count pool with free slots still marked free
  ignores its fds and spins forever after children exit.
- **Native port gates ≠ benchmark runtime gates:** port validation proves
  `bin/hb`, target source selection, syscalls/env, ELF AOT, checker/lints,
  self-refresh, REPL, Habu tooling on the target. Keep JS/Python/Rust/TS and live
  model runs under `bench/llm/run.f` on the Mac bench host, off small port targets.
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
- **Focused lint/check reruns copy the gate load list:** lint tools have non-obvious
  deps (`tools/date.f`, `lib/memory.f`, `lib/vector.f`, `tools/lint/intern.f`).
  Copy the `TEST-SUITE` list from `test/gate-stdlib.f` or the tool header instead
  of reconstructing from memory.
- **Created list DSLs run directly:** pass `[: ITEM ;] NAME-FILES`, matching
  `GE-FILES:`/`CHK-FILES:`. A generic `execute` wrapper needs a higher-order
  effect the checker does not model and fails before testing the list.
- **Checked dispatch rows need typed quotations:** in `tools/gate-json-assert.f`,
  raw `[']` row actions reached `GJA-DISPATCH-ONE-FILE-ROW` as an untyped `n`
  (`expected: ptr u8 n [ ptr u8 n-- ] actual: ptr u8 n n`). Use `[: ACTION ;]`
  rows when a checked row helper executes a mode-specific command.
- **Layout constants need one owner:** load `src/habu/layout.f` before every
  runtime prefix (env, baked REPL, stepper, watch, debug). Refresh once with
  compatibility constants if the installed `bin/hb` needs them, then remove dups
  and prove with the regenerated binary.
- **Manifest policy is row data plus one validator:** doc and module-note
  requirements in `tools/stdlib-manifest-test.f` stay maintainable when grouped
  as checked rows; branch ladders hide policy drift and make adding requirements
  look like control-flow work.

## VCS

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
  blobs via `refs/jj/keep` and `.jjconflict-*`. Ignore generated benchmark output
  by shape/dir (JSON/JSONL/logs/reports at `bench/llm/` or `bench/llm/results/`),
  not run name, so reruns can't enter a change.

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
- **Emitter punctuation is semantic:** `BL,`, `LBL,`, `ADR,`, `ZBYTES,` are
  distinct words; a dropped comma is a different token, surfacing as a terse
  undefined-token exit in generated stage2. Source-shape regressions assert exact
  punctuated tokens. Emitter stack comments describe the host/build stack
  (`( -- )`, `( n -- )`); keep emitted runtime effects in adjacent prose.
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
  success. `SPAWN-IO` uses a close-on-exec error pipe: child writes one byte before
  exiting on `chdir`/`dup2`/`execve` failure; parent reads EOF = success, or reads
  the byte, reaps, returns `-1`. Copy the fd to x0 before reusing that register for
  the marker byte. Else checked `SPAWN-IO` returns a pid for a missing exe.
- **Baked REPL needs explicit hook boundaries:** the snapshot prepends `0 set-check`
  then reinstalls a `CHECK!` hook before user input. The standard prefix's
  `roles.f` restores `' HOOK set-check`, so a baked tty bundle emits another
  `0 set-check` after the prefix and before `repl-term.f`/`repl.f`/watch/stepper/
  debug — without it the tty path rejects audited debug words (rc 70) and echoes
  PTY input with no prompt. Core fixtures (`src/core/sha256.f`) don't disable
  checking themselves; put `0 set-check` in the harness.
- **`--load` leaves stdin as tool data:** so a post-load probe piped to fd0 doesn't
  run — put capacity probes in an explicit loaded source file when measuring
  `here`/metadata. Gate load lists factor into `TEST-SUITE … ;TEST-SUITE` blocks
  with short lines (long physical lines hit the reader buffer).
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
- **M3 spike proven — Habu-emitted saxpy runs on the Orin GPU, bit-exact:**
  `tools/ptx/saxpy.f` emits `.version 8.3`/`.target sm_87` PTX, `ptxas` (CUDA 12.6)
  assembles it, the cubin matches a CPU golden 0/1000. Bit-exactness needs matching
  rounding: `mul.rn.f32`+`add.rn.f32` (two roundings; SASS `FMUL`+`FADD`, not
  `FFMA`) vs a golden compiled `-ffp-contract=off`.

## Linux AOT / ELF

- **Linux AOT gates parse ELF structure:** inspect ELF64 program headers and
  validate the executable `PT_LOAD` segment; Mach-O text-size thresholds aren't
  portable constants.
- **Instruction disassemblers read instruction width:** `DISASM` loads one ARM64
  u32 at a time; a u64-load-and-mask can cross a 4-byte mmap fixture end.
- **`die` is `( ptr u8 n n -- )`, no-return:** `0 0` is two ints, not a string;
  emit a real `s" msg"` or byte-backed word; old `s" msg" type cr 1 die` doesn't
  type-check, use `s" msg" code die`. Model process exits as branch-killing control
  flow through certified wrappers; record no-return metadata sparsely (only
  non-returning words + clearing redefinitions — a "returning" row per word adds
  startup pressure). `throw` is catchable and needs its own exception/catch effect,
  not the `die` model.

## Diagnostics & Benchmarks

- **Diagnostics are an API:** JSON errors carry `schema_version:1`, source spans,
  verdict, word, token, expected, actual; wrappers keep valid JSON object lines and
  fail nonzero on rejection. Source origins are wrapper-owned (definition-relative
  spans; inject origin markers, keep them out of user bundles). Repair/diagnostic
  rows keep a source-preserving effect field (`R x -- R x`) beside the normalized
  one; `fix_return_stack` only when the data stack already matches (a bad `>r` that
  drops a declared output is `add_producer` first).
- **JSON `s"` can't escape quotes:** build JSON/needles/rows with `lib/json-write.f`
  or checked byte/field helpers (explicit quote bytes), never escaped string
  literals. Cross-row JSON state copies `JSON-STRING$` bytes before the next
  `JSON-PARSE` (shared reusable buffer). Row artifacts stream via a chunked Habu
  emitter / OS-backed storage (`lib/memory.f`), never fixed-capacity builders or
  host encoders — prompt/response/replay fields exceed small buffers.
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
- **Never mix JSON stubs with runtime parser tests:** `bench/llm/diagnostic-json-check-stub.f`
  deliberately redefines `JSON-GET` to return missing data for checker-only
  fixtures; loading it beside real `tools/json.f` makes row assertions fail with
  fake `E-RA-MISSING`. Runtime fixtures load the real parser only.
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
  small `TEST-SUITE` and task-specific byte/source builders in the owning
  test/driver until multiple users prove shared surface — appending to large
  live-driver bundles surfaces as an unrelated rc 76 capacity failure. Load
  transitive deps in child bundles (`lib/build.f` needs `lib/process.f` for
  `RUN-RC`). Large native tool bundles (lint tables + `json.f` + big buffers) can
  corrupt reads — lean standalone reader + distinct scratch vars; stream large
  JSONL in chunks, reserve fixed buffers only for bounded summaries.
- **Captured gate children need heartbeat polls:** a silent child looks hung if
  `poll` waits the full timeout; gate capture loops poll in heartbeat slices,
  print label-only wait lines, keep child stdout/stderr for failures. Progress
  must exist at every blocking layer — a top-level `WAIT-RC` blinds the user even
  if children print heartbeats.
- **Gate heartbeat capture has one owner:** `lib/test-runner.f` owns
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
- **Source-use guards match exact tokens:** required-word checks compare whole
  source tokens, not substrings (`PROP-DEFAULTS` matched `PROP-DEFAULTS-OK?`;
  boundary scans skip comments/strings, else `ENTRUSTED-VALUE` false-rejects).
  Keep exact-token scanner helpers in a separate file loaded only by drivers that
  need them (folding into the base bundle tips file/process tests into capacity
  failure). `0<>` isn't a Habu word — use `0 = 0=`.
- **Name numeric buffer-room inputs:** effects like `( cap used add -- )` are all
  `n`; a stray `swap` type-checks but inverts capacity/add. Bind `add` at the
  caller, pass `cap used add` explicitly. Emitter helpers own their output buffer
  (a numeric renderer from another snippet type-checks while appending to the wrong
  global buffer). Name decoder shift constants (`BYTE-BITS 7 *` destabilizes
  locals-heavy defs); keep decoder bodies straight-line.
- **Shared tool libraries end checked:** `tools/lint/lib.f` must not leave callers
  in unchecked mode; downstream scanners declare+test any unchecked boundary
  locally and reinstall `CHECK!` immediately after raw declarations. Immediate-word
  fixtures (`postpone`/`compile,`) execute during compilation — keep wrappers
  inside the tested boundary, then restore `CHECK!`. Replay fixtures keep the
  first-bad path immutable (don't overwrite with a repaired candidate). Deep
  factoring/comment-only fixes record `file:line` findings in a durable doc
  (`docs/factorization-review.md`) and prove with focused source loads +
  `trust-lint` + the native gate.
- **Commit is a gate, not a checkpoint:** the project rule now treats `jj commit`
  as blocked until changed Forth diffs are scanned for definitions and unchecked
  boundaries, exact owning `bin/hb --load ...` paths are checked, and boundary
  tests/dots exist. This prevents "commit first, typecheck later" drift.
- **Gate phase files need their loader prefix:** `test/gate-stdlib.f` is not a
  standalone script; running it without the documented `docs/bootstrap.md`
  `--load` prefix omits `lib/process-env.f` and fails at
  `PROC-ENV-INHERIT-MISSING` before testing changed code. Use the exact native
  gate command for commit evidence.
