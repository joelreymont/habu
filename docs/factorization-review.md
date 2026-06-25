# Factorization Review

Date: 2026-06-25

This is the durable all-repo factorization audit record for the Linux/aarch64
port work. It focuses on large words, repeated stack juggling, missing
definition-local stack effects, repeated process/build command construction, and
places where checked Habu code should be split into smaller typed words.

The previous close-out did not leave this artifact in the repo. That was a
process miss: an all-repo factoring review must be written down with evidence,
agent scope, and remaining work.

## Agent Launch Record

All agents were read-only and ran with cwd `/home/user/Work/habu`.

| Agent | ID | Scope |
| --- | --- | --- |
| Archimedes | `019efe2d-343a-7833-9f29-bb17aa3410da` | `src/habu/habu1.f`, `src/habu/habu2.f`, build drivers, compiler/engine files |
| Euclid | `019efe2d-34a4-7450-b4b3-360d792f20dc` | `bootstrap/cg/*.fs`, `tools/bootstrap*.f` |
| Fermat | `019efe2d-355c-71a0-ad22-41305321a82b` | `tools/**/*.f`, `tools/*.f`, `test/**/*.f`, `test/*.f` |
| Lagrange | `019efe2d-6da6-78b0-b232-f86689c7a592` | `lib/*.f`, `src/arch/**/*.f`, `src/os/**/*.f` |

## Findings

| ID | Severity | Evidence | Root Cause | Required Fix |
| --- | --- | --- | --- | --- |
| F01 | High | `bootstrap/cg/jit.fs:240` | `EMIT-VBINIPREP` is a copy-expanded variant of `EMIT-VBINPREP`, repeating depth checks, tag/value loads, top forcing, register-freeing, pop, and fold loading. | Extract emitted helper fragments for VS tag/value access, forced top/deep handling, rm-bit free/pop, and fold loading; make the two words mode selectors. |
| F02 | High | `bootstrap/cg/forth.fs:2299` | `EMIT-INTERPRET-COLON` bundles token recognition, capacity checks, pending dict setup, name storage, signature capture, compile-state reset, and prologue emission. | Split colon-open phases into focused words such as colon token check, code/dict capacity, pending record init, optional signature capture, state reset, and prologue emission. |
| F03 | High | `src/arch/arm64/asm.f:6`, `src/arch/arm64/mnem.f:11`, `src/arch/arm64/icode.f:9`, `src/os/macos/macho.f:12`, `src/os/linux/elf.f:11` | Platform/arch encoder files still have many definitions without formal `( in -- out )` comments. | Add definition-local stack effects to every definition; factor repeated encoder signatures into shared wrappers where this reveals duplication. |
| F04 | Medium | `src/habu/habu1.f:473` | Darwin `BSPAWNIO` variants duplicate frame setup, file actions, descriptor zeroing, spawn, success/failure handling, and cleanup. | Factor shared Darwin spawn helpers; keep wrappers only for arity/register mapping. |
| F05 | Medium | `src/habu/habu2.f:25` | `C-CALL` mixes prologue recognition, inline policy, unsafe-instruction scan, inline copy, and absolute-call emission. | Split into prologue predicate, inline-safety predicate, inline copy, and absolute-call emitter helpers. |
| F06 | Medium | `src/habu/habu1.f:1333`, `bootstrap/cg/forth.fs:783` | Native and bootstrap `EMIT-NUM` words both bundle sign/base parsing, digit classification, integer/fraction accumulation, float conversion, and final return shape. | Factor prefix parsing, digit step, fraction step, integer finish, float finish, and fail return in both mirrors. |
| F07 | Medium | `bootstrap/cg/jit.fs:144` | `EMIT-VMOVK` bundles MOVZ/MOVN/MOVK selection, chunk counting, first-instruction selection, continuation emission, and all-zero/all-one fallbacks. | Factor chunk extraction/counting and first/continuation move-wide emitters. |
| F08 | Medium | `bootstrap/cg/forth.fs:1329`, `bootstrap/cg/forth.fs:1055` | Prefix file bytes and host load sequence are manually mirrored. | Add one prefix-file table/DSL that emits both path labels and load sequence; split target file selection by OS. |
| F09 | Medium | `bootstrap/cg/forth.fs:186`, `bootstrap/cg/regstack.fs:206`, `bootstrap/cg/jit.fs:144` | Raw bootstrap emitter/token-handler definitions lack definition-local stack effects. | Add exact stack effects, including `( -- )` for token wordlist entries, before locals/prose comments. |
| F10 | Medium | `src/os/macos/macho.f:7`, `src/os/linux/elf.f:6`, `src/os/macos/sign2.f:34` | ELF, Mach-O, and signing duplicate byte cursor/store vocabulary. | Factor a shared typed byte writer/reader layer with endian stores and cursor copy/pad words. |
| F11 | Medium | `lib/fs.f:396`, `lib/fs-mutate.f:97` | Walk and remove-tree duplicate directory traversal mechanics. | Factor directory iteration, child path enter/leave, and close handling into `lib/fs.f`; keep deletion policy in `lib/fs-mutate.f`. |
| F12 | Medium | `lib/process.f:235`, `lib/process-argv.f:140`, `lib/process-env.f:161` | Plain, argv, stdin, argv+env captures duplicate setup, probe, drain, poll, close, and reap lifecycle. | Factor shared capture validation/read/probe/drain/finish helpers while preserving the existing module split. |
| F13 | Medium | `tools/check.f:318` | `CHK-ARGV-*` words rebuild near-identical `--load` lists and expose static scanner phases mostly as child CLI recipes. | Introduce checked load-group/command builder words and split static linter cores from CLI wrappers so only true boundaries spawn. |
| F14 | Medium | `test/gate-stdlib.f:31`, `test/run.f:69` | Heartbeat capture logic is duplicated; stdin capture reaches into low-level process internals. | Add shared progress-aware capture helpers with stdin and non-stdin variants; keep gate words scenario-level. |
| F15 | Medium | `tools/stdlib-manifest-test.f:329`, `tools/stdlib-manifest-test.f:390` | Manifest documentation policy is a hard-coded imperative checklist and branch ladder. | Represent required docs and module note rules as checked rows and drive one row validator. |
| F16 | Medium | `tools/stale-status-lint.f:315` | `SS-COUNT-LINE?` mixes digit runs, ratio parsing, whitespace skipping, and keyword checks. | Split into scanner helpers for digits, ratio counts, count keywords, and whitespace. |
| F17 | Low | `src/habu/habu2.f:1624`, `src/habu/habu2.f:1021` | Optional signature scanning in `EM-INTERPRET-COLON` resembles `C-PARSE-TRUST-SIG`. | Factor shared signature scanner/capture helper with required and optional entry points. |
| F18 | Low | `src/habu/habu2.f:1774`, `src/habu/habu2.f:2048`, `src/habu/habu2.f:2050` | Table-like dispatch/data is encoded as long inline chains and long section lines. | Split by concern or introduce a checked dispatch/list DSL. |
| F19 | Low | `bootstrap/cg/elf.fs:10`, `bootstrap/cg/macho.fs:17` | Bootstrap ELF and Mach-O duplicate image buffer writer words. | Move common bootstrap image-buffer emitters into one shared file. |
| F20 | Low | `bootstrap/cg/asm-checked.fs:7` | Checked ARM64 encoders repeat instruction layout arithmetic. | Add checked layout combinators such as register/register, immediate, move-wide, and load/store helpers. |
| F21 | Low | `bootstrap/cg/forth.fs:1524` | Trust support mixes lookup/failure policy and raw call argument pushing. | Split required trust lookup, argument pushing, and generic save-LR call helper. |
| F22 | Low | `lib/regex.f:39`, `lib/regex.f:137`, `lib/regex.f:399`, `lib/regex.f:442` | Regex classification and state transitions are long ad hoc `dup`/`over` token chains. | Factor token predicates or a small table, then split scanner dispatch and state update helpers. |
| F23 | Low | `tools/gate-json-assert.f:283`, `tools/gate-json-assert.f:468` | Repair suggestion and command dispatch are branch ladders. | Use table-driven rows for command arity/xt and repair-class suggestion mapping. |
| F24 | Low | `test/engine-suite.f:7`, `tools/imgdump.f:63` | Several test/dump definitions lack stack-effect comments and some project words are lower-case. | Add comments before locals and rename project words to uppercase. |

## Already Addressed In The Port Stack

The preceding port/factoring commits already handled several issues found during
the first review round:

- build driver image writes now go through checked `src/habu/driver-io.f`;
- bootstrap source/body buffer handling fails closed;
- bootstrap arity/effect parsing no longer defaults on parse failure;
- disassembly reads instruction-width words;
- imagedisasm and bootstrap codegen regressions are in the gate;
- stale trust rows and several unchecked pointer reloads were removed.
- F03 platform/arch definition-local stack effects were added for
  `src/arch/arm64/asm.f`, `src/arch/arm64/mnem.f`,
  `src/arch/arm64/icode.f`, `src/os/linux/elf.f`,
  `src/os/macos/macho.f`, and `src/os/macos/sign2.f`; focused source loads,
  `trust-lint`, and the full native gate passed.
- F02 bootstrap colon-open emission was split into named phase helpers in
  `bootstrap/cg/forth.fs`; `tools/bootstrap-codegen-test.f` and the full native
  gate passed. No-binary recovery bootstrap was not run on this host because the
  installed Gforth 0.7.3 fails the documented `{:` locals probe and
  `tools/bootstrap.sh` exits 69 before touching `bin/hb`.
- F01 mirrored VBIN prep emission was split into shared helper fragments in
  `src/habu/jit.f` and `bootstrap/cg/jit.fs`; `tools/bootstrap-codegen-test.f`,
  `trust-lint`, and the full native gate passed. No-binary recovery bootstrap
  was not run on this host because the installed Gforth 0.7.3 fails the
  documented `{:` locals probe and `tools/bootstrap.sh` exits 69 before touching
  `bin/hb`.
- F09 bootstrap definition-local stack effects were completed for
  `bootstrap/cg/forth.fs`, `bootstrap/cg/regstack.fs`, and
  `bootstrap/cg/jit.fs`; the F09 scans, `tools/bootstrap-codegen-test.f`,
  `trust-lint`, and the full native gate passed. No-binary recovery bootstrap
  was not run on this host because installed Gforth 0.7.3 fails the documented
  `{:` locals probe and `tools/bootstrap.sh` exits 69 before touching `bin/hb`.
- F06 native and bootstrap numeric parsing mirrors were split into phase helpers
  in `src/habu/habu1.f` and `bootstrap/cg/forth.fs`; numeric literal coverage
  was added for hex, negative hex, and negative float literals. The focused
  engine suite, `tools/bootstrap-codegen-test.f`, `trust-lint`, the focused
  native engine gate, and the full native gate passed. No-binary recovery
  bootstrap was not run on this host because installed Gforth 0.7.3 fails the
  documented `{:` locals probe and `tools/bootstrap.sh` exits 69 before touching
  `bin/hb`; `bin/hb` kept sha256
  `cba97dd68f37e1b9f7eacb90cc17b3c3c93717c335d77f8352a1e4e7bba33a7c` before
  and after the recovery probe.
- F07 native and bootstrap move-wide JIT emitters were split into helper phases
  for frame setup, initialization, chunk counting, form selection, chunk load,
  skip checks, first MOVZ/MOVN emission, MOVK continuation emission, fallback,
  and return. `test/engine-suite.f` now exercises compiled literal
  materialization for zero, all-ones, MOVZ/MOVK, and MOVN/MOVK forms.
  `tools/bootstrap-codegen-test.f`, `bin/hb test/engine-suite.f`,
  `trust-lint`, and the full native gate passed. No-binary recovery bootstrap
  was not run on this host because installed Gforth 0.7.3 fails the documented
  `{:` locals probe and `tools/bootstrap.sh` exits 69 before touching `bin/hb`;
  `bin/hb` kept sha256
  `4aa06fe536a15961f24a2a1d75a2678c3da7d017c5295b72987c285f2536de92` before
  and after the recovery probe.
- F08 native and bootstrap source prefixes now use one `PFX-FILES` row list with
  separate row actions for load emission and path-data emission in
  `src/habu/habu2.f` and `bootstrap/cg/forth.fs`. The regression in
  `tools/bootstrap-codegen-test.f` checks both mirrors and guards the
  punctuation-sensitive emitter calls `BL,` and `ZBYTES,`. During RCA the full
  gate first failed at `build-helper-fixtures` because the refactor had dropped
  those commas; focused nested refresh reproduced rc 14/70 on bare `BL` and then
  bare `ZBYTES`, and the committed fix rejects both typos in the source test.
  `trust-lint`, `tools/bootstrap-codegen-test.f`, `bin/hb test/engine-suite.f`,
  the focused `build-helper-fixtures` load bundle, and the full native gate
  passed. No-binary recovery bootstrap was not run on this host because
  installed Gforth 0.7.3 fails the documented `{:` locals probe and
  `tools/bootstrap.sh` exits 69 before touching `bin/hb`; `bin/hb` kept sha256
  `d2d79b59c70a4de0d160b886ededde6941a92feb29bef3b105d26800b1d3793b`
  before and after the recovery probe.
- F21 native and bootstrap trust-call emitters were split into `C-PUSH-DATA-CELL`,
  `C-PUSH-TRUST-SIG`, and `C-CALL-X11-SAVED`, leaving
  `C-CALL-TRUST-LASTC` and native-only `C-CALL-TRUST-PEND` as small scenario
  words. `tools/bootstrap-codegen-test.f` now guards the helper shape and rejects
  the old raw `CRSIG`/`TSIG` push sequences. `tools/bootstrap-codegen-test.f`,
  `trust-lint`, `stale-status-lint`, `bin/hb test/engine-suite.f`, the focused
  `build-helper-fixtures` load bundle, and the full native gate passed.
  No-binary recovery bootstrap was not run on this host because installed Gforth
  0.7.3 fails the documented `{:` locals probe and `tools/bootstrap.sh` exits 69
  before touching `bin/hb`; `bin/hb` kept sha256
  `50a6cfe5fb69e80f46db229bc7d8c8414e91de7b0e02a12e5c43f5bb1a131b56`
  before and after the recovery probe.
- F19 bootstrap ELF and Mach-O writers now share the executable image buffer
  vocabulary in `bootstrap/cg/image.fs`. `bootstrap/cg/elf.fs` and
  `bootstrap/cg/macho.fs` keep only target header/layout policy and both append
  assembled code through `M-BYTES`. `tools/bootstrap-codegen-test.f` guards that
  the target files require the shared buffer and do not redeclare `MSIZE`,
  `MBUF`, `MP`, `MLEN`, or the byte/word store helpers. `tools/bootstrap-codegen-test.f`,
  `trust-lint`, `stale-status-lint`, `bin/hb test/engine-suite.f`, the focused
  `build-helper-fixtures` load bundle, and the full native gate passed.
  No-binary recovery bootstrap was not run on this host because installed Gforth
  0.7.3 fails the documented `{:` locals probe and `tools/bootstrap.sh` exits 69
  before touching `bin/hb`; `bin/hb` kept sha256
  `3e26563a5bb47ca142f56b3a245a9d9f5c54ac5de171476ecdce024a10d0cae8`
  before and after the recovery probe.
- F20 checked ARM64 encoders now use shared instruction-layout combinators in
  `bootstrap/cg/asm-checked.fs` for RRR16, RRI10, move-wide, unsigned-offset
  load/store, branch-register, and CSET layouts. `tools/asm-checked-test.f`
  checks the helper layouts and all existing encoder words against known ARM64
  instruction words; `tools/bootstrap-codegen-test.f` guards against returning
  to repeated raw `lshift`/`swap` layouts, and the fixture is part of the
  `build-helper-fixtures` gate batch. `tools/bootstrap-codegen-test.f`,
  `tools/asm-checked-test.f`, `trust-lint`, `stale-status-lint`,
  `bin/hb test/engine-suite.f`, the focused `build-helper-fixtures` bundle, and
  the full native gate passed. No-binary recovery bootstrap was not run on this
  host because installed Gforth 0.7.3 fails the documented `{:` locals probe and
  `tools/bootstrap.sh` exits 69 before touching `bin/hb`; `bin/hb` kept sha256
  `c7a2bdac0ac2c10bfd65cff251a8ba204f727803514524520a50ad9dbd77b4bb`
  before and after the recovery probe.
- F10 native ELF, Mach-O, and signing code now share the executable image byte
  cursor and endian patch vocabulary in `src/os/image-bytes.f`. Linux
  `elf.f` and macOS `macho.f` keep only format layout policy, while macOS
  `sign2.f` uses shared little-endian header patch helpers and big-endian
  signature blob writers. `tools/image-bytes-test.f` checks little-endian
  stores, absolute patch reads/writes, copy/pad/name helpers, big-endian
  signature writes, and source-shape guards that reject the removed local cursor
  definitions. `tools/build-fixpoint.f`, `tools/srclist.f`, `FILEMAP.md`,
  `tools/filemap-lint.f`, and `tools/lint/shadow-lint.f` all include the shared
  source. `tools/image-bytes-test.f`, `trust-lint`, `stale-status-lint`,
  `filemap-lint`, `shadow-lint`, the focused `build-helper-fixtures` bundle, and
  the full native gate passed. No-binary recovery bootstrap was not run on this
  host because installed Gforth 0.7.3 fails the documented `{:` locals probe and
  `tools/bootstrap.sh` exits 69 before touching `bin/hb`; `bin/hb` kept sha256
  `fd83258137f0c679a6d738378beebe8e437a724d367fbd1a9759a6fb1a61f371`
  before and after the recovery probe.
- F04 source factoring is implemented in `src/habu/habu1.f`: the Darwin
  `BSPAWNIO` variants now share frame enter/leave, action reset, stdio dup2
  append, descriptor zero/fill, nullable descriptor, argv/envp register setup,
  and `posix_spawn` finish helpers. `tools/spawn-emitter-test.f` guards the
  source shape and old duplicated literal sequences. On Linux/aarch64,
  `tools/spawn-emitter-test.f`, process/process-argv/process-env/process-cwd
  focused fixtures, `trust-lint`, `filemap-lint`, `stale-status-lint`, the
  focused build-helper bundle, the full native gate, and the local recovery
  probe passed as far as this host can prove. F04 remains open because this host
  cannot run the required macOS process tests and macOS full native gate.
- F05 native `C-CALL` emission is split in `src/habu/habu2.f`: prologue
  recognition, prologue/plain inline-span selection, return-slot validation,
  masked/exact unsafe-instruction rejection, safe-body scanning, inline copying,
  and absolute-call stencil emission are separate helper words with explicit
  stack effects. `tools/c-call-emitter-test.f` guards the source shape and old
  inline duplication. `tools/c-call-emitter-test.f`, `bin/hb
  test/engine-suite.f`, `tools/bootstrap-codegen-test.f`, `trust-lint`,
  `stale-status-lint`, `filemap-lint`, and the full native gate passed.
  No-binary recovery bootstrap was not run on this host because installed Gforth
  0.7.3 fails the documented `{:` locals probe and `tools/bootstrap.sh` exits
  69 before touching `bin/hb`; `bin/hb` kept sha256
  `a09a95574b1a185a7ec918d33b84fce839fd623441339f006366ac1eac2da7fd`
  before and after the recovery probe.
- F17 native and recovery signature scanning now use shared scanner/capture
  emitters. `src/habu/habu2.f` factors whitespace/open-paren scanning,
  close-paren scanning, inner/full signature spans, `TSIG-*` capture/body append,
  and the required-signature error path; `C-PARSE-TRUST-SIG` delegates to the
  required scanner and `EM-INTERPRET-COLON` calls `C-COLON-MAYBE-SIG` instead of
  embedding its own scanner. `bootstrap/cg/forth.fs` uses the same shared
  scanner vocabulary for recovery created-word and optional colon signatures.
  `tools/signature-scan-emitter-test.f` guards both source shapes and rejects the
  old inline scanner blocks. `tools/signature-scan-emitter-test.f`,
  `tools/bootstrap-codegen-test.f`, the focused build-helper bundle, dictionary
  and diagnostics gates, the AOT negative signature gate, `trust-lint`,
  `stale-status-lint`, `filemap-lint`, `shadow-lint`, `bin/hb
  test/engine-suite.f`, and the full native gate passed. No-binary recovery
  bootstrap was not run on this host because installed Gforth 0.7.3 fails the
  documented `{:` locals probe and `tools/bootstrap.sh` exits 69 before touching
  `bin/hb`; `bin/hb` kept sha256
  `a81b96e5501123cc0a42f8cc6beb9442ac78851b61fdb50ac488eb8bbf373879`
  before and after the recovery probe.
- F18 native and recovery compiler dispatch/data chains are split by concern:
  interpreter defining/string/number/lookup dispatch, compile keyword groups,
  optimized operator groups, and code-section groups now have separate helper
  words in `src/habu/habu2.f` and `bootstrap/cg/forth.fs`.
  `tools/compiler-dispatch-test.f` guards the helper definitions, call graph,
  and removal of the old long section chains. `tools/compiler-dispatch-test.f`,
  `tools/bootstrap-codegen-test.f`, the focused build-helper bundle,
  `trust-lint`, `stale-status-lint`, `filemap-lint`, `shadow-lint`, `bin/hb
  test/engine-suite.f`, and the full native gate passed. No-binary recovery
  bootstrap was not run on this host because installed Gforth 0.7.3 fails the
  documented `{:` locals probe and `tools/bootstrap.sh` exits 69 before touching
  `bin/hb`; `bin/hb` kept sha256
  `a5815a15d473ef295d1bda02e06d89a2dd45aba8eb273c6aed343ae223fa25a1`
  before and after the recovery probe.
- F12 process capture setup, nonblocking probe/drain, optional stdin write,
  timeout polling, fd cleanup, and finish helpers now live in `lib/process.f`.
  `lib/process-argv.f`, `lib/process-env.f`, and `lib/process-cwd.f` prepare
  argv/env/cwd state and delegate the shared capture lifecycle. The
  process/process-argv/process-env/process-cwd focused fixtures, affected
  `tools/check.f --source-list` bundles, `tools/stdlib-manifest-test.f`,
  `tools/public-signatures-test.f`, `trust-lint`, `stale-status-lint`,
  `filemap-lint`, `shadow-lint`, `bin/hb test/engine-suite.f`, and
  `test/gate-stdlib.f` passed; the full native gate passed after the batch.
- F14 progress-aware capture now lives in `lib/test-runner.f`. The shared
  helpers drain captured stdout/stderr into gate buffers, optionally flush
  complete child-output lines for top-level phases, and drive stdin captures
  through the `lib/process.f` stdin poll/drive helpers. `test/gate-stdlib.f`
  and `test/run.f` keep only phase setup and pass/fail policy. The stale direct
  polling scan was clean, `lib/test-runner-test.f`, `tools/check.f
  --source-list`, `tools/stdlib-manifest-test.f`, `test/gate-stdlib.f`, and the
  full native gate passed.

## Continuation Handoff

Tracker state was verified with `dot tree` on 2026-06-25 during the F04
source-refactor handoff. The parent dot is `habu-review-whole-repo-5e087327`;
F01, F02, F03, F05, F06, F07, F08, F09, F10, F12, F13, F14, F16, F17, F18,
F19, F20, and F21 are addressed; F04 has a Linux-validated source refactor but
remains open for macOS runtime validation; all rows below are open.

The local `.dots/` store is ignored by the repository, so this section is the
durable committed queue. A fresh checkout can recreate equivalent dots from the
tables below if the local dot store is unavailable. Use one parent dot for the
whole-repo factorization review, one child dot per open finding, and the three
non-factorization top-level dots listed below. For each finding dot, copy the
Finding row's evidence, root cause, and required fix into the dot description,
then add the validation from the "Done when" column. Do not create duplicate
dots when the local store already contains the IDs below.

Handoff snapshot:

- The F04 source-refactor edits were validated on Linux/aarch64 by
  `tools/spawn-emitter-test.f`, process/process-argv/process-env/process-cwd
  focused fixtures, `trust-lint`, `stale-status-lint`, `filemap-lint`,
  `shadow-lint` through the full gate, the focused `build-helper-fixtures`
  bundle, the full native gate, and the local recovery probe described above.
- The F05 source-refactor edits were validated on Linux/aarch64 by
  `tools/c-call-emitter-test.f`, `bin/hb test/engine-suite.f`,
  `tools/bootstrap-codegen-test.f`, `trust-lint`, `stale-status-lint`,
  `filemap-lint`, the full native gate, and the local recovery probe described
  above.
- The F17 source-refactor edits were validated on Linux/aarch64 by
  `tools/signature-scan-emitter-test.f`, `tools/bootstrap-codegen-test.f`, the
  focused build-helper bundle, dictionary and diagnostics gates, the AOT
  negative signature gate, `trust-lint`, `stale-status-lint`, `filemap-lint`,
  `shadow-lint`, `bin/hb test/engine-suite.f`, the full native gate, and the
  local recovery probe described above.
- The F18 source-refactor edits were validated on Linux/aarch64 by
  `tools/compiler-dispatch-test.f`, `tools/bootstrap-codegen-test.f`, the
  focused build-helper bundle, `trust-lint`, `stale-status-lint`,
  `filemap-lint`, `shadow-lint`, `bin/hb test/engine-suite.f`, the full native
  gate, and the local recovery probe described above.
- The F12 process-capture edits were validated on Linux/aarch64 by the
  process/process-argv/process-env/process-cwd focused fixtures, affected
  `tools/check.f --source-list` bundles, `tools/stdlib-manifest-test.f`,
  `tools/public-signatures-test.f`, `trust-lint`, `stale-status-lint`,
  `filemap-lint`, `shadow-lint`, `bin/hb test/engine-suite.f`, and
  `test/gate-stdlib.f`; the full native gate also passed.
- The F14 gate-progress edits were validated on Linux/aarch64 by the stale
  direct polling scan, `lib/test-runner-test.f`, affected `tools/check.f
  --source-list`, `tools/stdlib-manifest-test.f`, `test/gate-stdlib.f`, and the
  full native gate.
- F13 has a first sub-batch: `tools/check.f` now centralizes child `--load`
  recipes behind checked `CHK-FILES:` load groups and small command-builder
  words instead of rebuilding the same argv prefixes in each `CHK-ARGV-*`
  routine. `tools/check-test.f` and the stdlib gate `check-cli-boundary` phase
  passed, and the full native gate passed after the sub-batch. F13 remains
  open: the remaining work is splitting static scanner cores from their CLI
  wrappers so check phases that are not true process boundaries can run
  in-process.
- F13 has a second sub-batch: `tools/diag-origin-core.f` now owns the reusable
  marker injection core, `tools/diag-origin.f` is only the CLI wrapper, and
  `tools/check.f` calls `DIAG-ORIGIN>BUF` in-process instead of spawning an
  `hb` child for origin marker insertion. Validation passed on Linux/aarch64:
  `tools/diag-origin-test.f`, `tools/check-test.f`, direct `tools/check.f`
  good/bad JSON smokes, `filemap-lint`, `trust-lint`, `stale-status-lint`,
  `shadow-lint`, `tools/hb-build-test.f`, focused native bench helper tests
  covering the changed checker argv builders, and the full native gate from
  `docs/bootstrap.md`.
- F13 has a third sub-batch: `tools/json-only-core.f` now owns the reusable JSON
  diagnostic filter, `tools/json-only.f` is only the CLI file/argv wrapper, and
  `tools/check.f` filters captured checker stderr in-process instead of writing
  a temp stderr file and spawning `hb` only to run `tools/json-only.f`.
  Validation on Linux/aarch64 covered `tools/json-only-test.f`,
  `tools/check-test.f`, direct `tools/check.f` good/bad JSON smokes, affected
  native benchmark helper fixtures (`run-test`, `perf` stub,
  `run-attempts` focused/CLI, `drive-forth`, and `drive-array-habu`), and
  `filemap-lint`/`trust-lint`/`stale-status-lint`/`shadow-lint`, followed by
  the full native gate from `docs/bootstrap.md`.
- F13 has a fourth sub-batch: `tools/signature-lint-core.f` now owns the reusable
  strict typed-signature scanner, `tools/signature-lint.f` is only the CLI argv
  wrapper, and `tools/check.f --strict-signatures` calls the core in-process with
  stderr output instead of spawning a child `hb` only to run
  `tools/signature-lint.f`. Validation on Linux/aarch64 covered
  `tools/signature-lint-test.f`, `tools/check-test.f` with text and JSON strict
  signature regressions, affected native benchmark helper fixtures (`run-test`,
  `perf` stub, `run-attempts` focused/CLI/checker-safe, `drive-forth`, and
  `drive-array-habu`), `tools/hb-build-test.f`, and
  `filemap-lint`/`trust-lint`/`stale-status-lint`/`shadow-lint`, followed by the
  full native gate from `docs/bootstrap.md`.
- F13 has a fifth sub-batch: `tools/checked-boundary-lint-core.f` now owns the
  reusable unchecked-boundary scanner, `tools/checked-boundary-lint.f` is only the
  CLI argv wrapper, and `tools/check.f` calls the core in-process with stderr
  output instead of spawning a child `hb` only to run
  `tools/checked-boundary-lint.f`. Validation on Linux/aarch64 covered
  `tools/checked-boundary-lint-test.f`, `tools/check-test.f` with a strict
  unchecked-boundary regression, affected native benchmark helper fixtures
  (`run-test`, `perf` stub, `run-attempts` focused/CLI/checker-safe,
  `drive-forth`, and `drive-array-habu`), and
  `filemap-lint`/`trust-lint`/`stale-status-lint`/`shadow-lint`, followed by the
  full native gate from `docs/bootstrap.md`.
- F13 has a sixth sub-batch: `tools/trust-lint-core.f` now owns the reusable
  `TRUSTED.md` scanner, `tools/trust-lint.f` is only the CLI argv wrapper, and
  `tools/check.f` calls the core in-process with stderr output instead of
  spawning a child `hb` only to run `tools/trust-lint.f`. RCA found that simply
  loading the old static trust buffers into `tools/check.f` corrupted
  `SCRIPT-ARGC` before parse under the combined checker bundle; the core now uses
  caller-supplied scratch buffers, with the wrapper owning standalone buffers and
  check lending its existing run/origin buffers during the trust phase. Validation
  on Linux/aarch64 covered `tools/trust-lint-test.f`, `tools/check-test.f`,
  direct good-path checker smoke, `tools/checked-boundary-lint-test.f`,
  `tools/imagedisasm-test.f`, affected native benchmark helper fixtures
  (`run-test`, `perf` stub, `run-attempts` focused/CLI/checker-safe,
  `drive-forth`, and `drive-array-habu`), and
  `filemap-lint`/`trust-lint`/`stale-status-lint`/`shadow-lint`, followed by the
  full native gate from `docs/bootstrap.md`. At that point F13 remained open:
  all-errors still ran as a child CLI tool and needed a core/wrapper split or an
  explicit boundary classification.
- F13 has a seventh sub-batch: `tools/check-all-errors-core.f` now owns the
  reusable all-errors checker core, `tools/check-all-errors.f` is only the CLI
  argv wrapper, and `tools/check.f --all-errors` calls the core in-process
  instead of spawning a child `hb` only to run the wrapper. The core keeps
  caller-supplied stdout/stderr capture buffers so the checker bundle does not
  duplicate large static buffers, and the per-definition `bin/hb` runs remain
  classified as the true process boundary: each generated checked definition can
  fail/throw independently while the all-errors pass continues collecting later
  diagnostics. Validation on Linux/aarch64 covered `tools/check-all-errors-test.f`,
  `tools/check-test.f`, direct good and bad `tools/check.f --json-errors
  --all-errors` smokes, `test/gate-stdlib.f`, affected native benchmark helper
  fixtures (`run-test`, `run-attempts-check-test`,
  `run-attempts-cli-check-test`, `drive-forth-test`, `drive-array-habu-test`,
  and the stubbed `bench/llm/perf.f --json --full` CLI), followed by the full
  native gate from `docs/bootstrap.md`. This closes F13 on Linux/aarch64.
- F11 is closed on Linux/aarch64: directory traversal mechanics now live in
  checked helpers in `lib/fs.f` (`FS-SKIP-SELF-ENTRY?`, `FS-OPEN-WALK-DIR`,
  `FS-DIR-BLOCK-BEGIN`, `FS-DIR-MORE?`, `FS-LOAD-ENTRY`, `FS-ADVANCE-ENTRY`,
  `FS-DESCEND-PATH`, `FS-ASCEND-PATH`, and `FS-CLOSE-CUR-DIR`). `WALK-FILES`,
  `REMOVE-TREE`, and the attempt-runner round scanner reuse those helpers while
  keeping their policies separate: `WALK-FILES` still skips repo metadata, and
  `REMOVE-TREE` still descends into `.dots` while unlinking symlinks without
  following them. Validation covered `lib/fs-test.f`, `lib/fs-mutate-test.f`,
  `bench/llm/run-attempts-check-test.f`, `bench/llm/run-attempts-cli-check-test.f`,
  `bench/llm/run-attempts-test.f`, `tools/stdlib-manifest-test.f`,
  `tools/filemap-lint.f`, `test/gate-stdlib.f`, and the full native gate from
  `docs/bootstrap.md`. The manifest validator row cap was raised from 768 to
  1024 so the expanded checked stdlib inventory remains representable.
- F15 is closed on Linux/aarch64: `tools/stdlib-manifest-test.f` now drives
  documentation requirements through named checked doc-row groups and validates
  module-note requirements through one `SMT-CHECK-MODULE-NOTE-ROW` helper instead
  of a module branch ladder. The manifest format did not change. Validation
  covered the focused `tools/stdlib-manifest-test.f` command and the full native
  gate from `docs/bootstrap.md`.
- F16 is closed on Linux/aarch64: `tools/stale-status-lint.f` now splits live
  count detection into explicit cursor, digit-run, ratio-tail, whitespace, and
  keyword helpers (`SS-SCAN-DIGITS`, `SS-COUNT-RATIO?`, `SS-SKIP-WS`,
  `SS-COUNT-KEYWORD?`, and `SS-COUNT-CANDIDATE?`). `tools/stale-status-lint-test.f`
  now covers uncheckable and case-insensitive keyword counts, short counts,
  embedded alnum counts, partial ratios, fenced counts, and long markdown files.
  Validation covered the focused `tools/stale-status-lint-test.f` command,
  direct `stale-status-lint`, `test/gate-stdlib.f`, and the full native gate
  from `docs/bootstrap.md`.
- The remaining factorization work already has one dot per open finding in the
  local tracker. Do not create duplicates; start the next open row, commit that
  focused batch, update this document, close that row's dot, then push.
- The three non-factorization top-level dots listed below are also open and
  should stay separate from the all-repo factorization parent.
- Do not close the parent dot until every child row is closed and this document
  records the final validation.

Next continuation step:

1. Finish `habu-factor-darwin-spawn-5a82930c` on macOS by running the process,
   process-argv, process-env, process-cwd, PTY, and full native gates against
   the factored Darwin emitters.
2. If those macOS checks pass, update this document with the exact evidence,
   close F04, and continue to the next Linux-actionable row.
3. If macOS exposes a spawn behavior regression, keep F04 open, root-cause the
   register/frame delta with the native debugger tools in `docs/debugging.md`,
   and commit the fix with a macOS regression.
4. On Linux/aarch64, the next unblocked finding is F22
   (`habu-factor-regex-token-865ebac5`).

Open dot queue:

| Order | Finding | Dot | Scope | Done when |
| --- | --- | --- | --- | --- |
| 1 | F04 | `habu-factor-darwin-spawn-5a82930c` | Validate the factored Darwin spawn emitter variants on macOS. | Shared helpers preserve the Darwin `posix_spawn` ABI; macOS process tests and full native gate pass, plus Linux-safe gate where run. |
| 2 | F22 | `habu-factor-regex-token-865ebac5` | Factor regex token predicates or transitions. | Regex classification/state transitions use small predicate or row helpers; regex fixtures and full native gate pass. |
| 3 | F23 | `habu-table-drive-gate-698becb6` | Table-drive gate JSON command/repair dispatch. | Command arity/xt rows and repair suggestion rows replace branch ladders; gate-json assertions and full native gate pass. |
| 4 | F24 | `habu-clean-engine-imgdump-b5c63365` | Add comments and uppercase project words in engine/imgdump tests. | Missing stack-effect comments and lower-case project words are fixed; focused source loads, lint, and full native gate pass. |

## Other Open Top-Level Dots

These dots are outside the factorization parent and remain open in `dot ready`.

| Priority | Dot | Required Fix | Done when |
| --- | --- | --- | --- |
| 1 | `habu-replace-creates-with-d9c4b404` | Replace the project-specific `CREATES` marker with standard checked `CREATE ... DOES>`, update tests/docs/gate DSL users, and remove the legacy keyword path. | No `CREATES` keyword path or fallback remains; `CREATE ... DOES>` tests cover created-word effects; full native gate passes. |
| 2 | `habu-add-typed-byte-b25e923e` | Add a checked byte-pointer offset primitive/model so `lib/fs.f` can drop the trusted `FS-BYTE-OFFSET` boundary. | The trusted `FS-BYTE-OFFSET` boundary is gone; pointer-offset checker tests, fs dirent/stat tests, and full native gate pass. |
| 2 | `habu-model-engine-builder-38ddc643` | Model raw engine-builder asm/codegen effects under the hard hook so the generated `0 set-check` boundary can be removed. | Generated build sources no longer need the audited `0 set-check` bracket for engine-builder effects; build-fixpoint tests and full native gate pass. |

## Tracking Dots

Parent: `habu-review-whole-repo-5e087327`

| Finding | Dot |
| --- | --- |
| F01 | `habu-factor-bootstrap-vbin-53787869` |
| F02 | `habu-factor-bootstrap-colon-0ab81878` |
| F03 | `habu-add-arch-platform-4818d614` |
| F04 | `habu-factor-darwin-spawn-5a82930c` |
| F05 | `habu-factor-native-c-230e1316` |
| F06 | `habu-factor-mirrored-num-c2faa343` |
| F07 | `habu-factor-bootstrap-move-9722fa8e` |
| F08 | `habu-unify-bootstrap-prefix-26788bfa` |
| F09 | `habu-add-bootstrap-stack-a6b31511` |
| F10 | `habu-factor-typed-byte-b311d5c7` |
| F11 | `habu-factor-filesystem-traversal-f490595e` |
| F12 | `habu-factor-process-capture-467f9021` |
| F13 | `habu-factor-check-load-2e29d26a` |
| F14 | `habu-factor-gate-progress-555aa42d` |
| F15 | `habu-table-drive-stdlib-786cb080` |
| F16 | `habu-factor-stale-status-615b5a1b` |
| F17 | `habu-share-signature-scan-5353e68b` |
| F18 | `habu-factor-compiler-dispatch-0167f41a` |
| F19 | `habu-share-bootstrap-image-ef41b8f8` |
| F20 | `habu-factor-checked-arm64-f1f46265` |
| F21 | `habu-factor-bootstrap-trust-71f82afa` |
| F22 | `habu-factor-regex-token-865ebac5` |
| F23 | `habu-table-drive-gate-698becb6` |
| F24 | `habu-clean-engine-imgdump-b5c63365` |

## Verification Status

The original subagent review was read-only. The latest implementation batch is
the Linux-validated F16 stale-status scanner sub-batch. F11, F13, F15, and F16
are closed on Linux/aarch64; after the F16 sub-batch the port stack passed:

- `tools/stale-status-lint-test.f`: `stale-status-lint-test: ok`;
- direct `stale-status-lint`: `stale-status-lint: 0 finding(s)`;
- `test/gate-stdlib.f`: `PASS: native lint/stdlib gate phase`;
- `test/run.f`:
  `PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build)`.

## Agent Command Notes

Agents used `rg`, `sed`, `nl`, `wc`, and definition-length scans to locate large
or uncommented definitions and duplicated command/process patterns. One tools
scope used a one-off Python definition-length scanner during read-only analysis;
no host-language code was added to the repository.
