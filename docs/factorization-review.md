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

## Continuation Handoff

Tracker state was verified with `dot tree habu-review-whole-repo-5e087327`.
The parent dot is `habu-review-whole-repo-5e087327`; F01, F02, F03, F06, F07,
F08, and F09 are addressed; all rows below are open. No duplicate top-level dots
are needed.
The local `.dots/` store is ignored by the repository, so this section is the
durable committed queue. A fresh checkout can recreate the tracker from this
table if the local dot store is unavailable.

Next continuation step:

1. Start `habu-factor-bootstrap-trust-71f82afa`.
2. Split bootstrap trust lookup, argument pushing, and save-LR call helpers in
   `bootstrap/cg/forth.fs`, keeping exact stack effects on every helper.
3. Validate with the focused bootstrap codegen tests, `trust-lint`, the native
   fixpoint/full gate from `docs/bootstrap.md`, and the no-binary recovery probe
   when a Gforth with `{:` locals is available.

Open dot queue:

| Order | Finding | Dot | Scope |
| --- | --- | --- | --- |
| 1 | F21 | `habu-factor-bootstrap-trust-71f82afa` | Split bootstrap trust lookup, argument pushing, and save-LR call helper. |
| 2 | F19 | `habu-share-bootstrap-image-ef41b8f8` | Share bootstrap ELF/Mach-O image buffer emitters. |
| 3 | F20 | `habu-factor-checked-arm64-f1f46265` | Add checked ARM64 layout combinators. |
| 4 | F10 | `habu-factor-typed-byte-b311d5c7` | Factor shared ELF/Mach-O/signing byte cursor layer. |
| 5 | F04 | `habu-factor-darwin-spawn-5a82930c` | Factor Darwin spawn emitter variants. |
| 6 | F05 | `habu-factor-native-c-230e1316` | Split native `C-CALL` phases. |
| 7 | F17 | `habu-share-signature-scan-5353e68b` | Share required/optional signature scanning. |
| 8 | F18 | `habu-factor-compiler-dispatch-0167f41a` | Split compiler dispatch/data chains by concern or checked rows. |
| 9 | F12 | `habu-factor-process-capture-467f9021` | Factor capture setup, probe, drain, close, and reap lifecycle. |
| 10 | F14 | `habu-factor-gate-progress-555aa42d` | Share progress-aware capture helpers. |
| 11 | F13 | `habu-factor-check-load-2e29d26a` | Split check/load builders and keep only true boundary spawns. |
| 12 | F11 | `habu-factor-filesystem-traversal-f490595e` | Factor directory traversal mechanics. |
| 13 | F15 | `habu-table-drive-stdlib-786cb080` | Table-drive stdlib manifest documentation policy. |
| 14 | F16 | `habu-factor-stale-status-615b5a1b` | Split stale-status count scanner helpers. |
| 15 | F22 | `habu-factor-regex-token-865ebac5` | Factor regex token predicates or transitions. |
| 16 | F23 | `habu-table-drive-gate-698becb6` | Table-drive gate JSON command/repair dispatch. |
| 17 | F24 | `habu-clean-engine-imgdump-b5c63365` | Add comments and uppercase project words in engine/imgdump tests. |

## Other Open Top-Level Dots

These dots are outside the factorization parent and remain open in `dot ready`.

| Priority | Dot | Required Fix |
| --- | --- | --- |
| 1 | `habu-replace-creates-with-d9c4b404` | Replace the project-specific `CREATES` marker with standard checked `CREATE ... DOES>`, update tests/docs/gate DSL users, and remove the legacy keyword path. |
| 2 | `habu-add-typed-byte-b25e923e` | Add a checked byte-pointer offset primitive/model so `lib/fs.f` can drop the trusted `FS-BYTE-OFFSET` boundary. |
| 2 | `habu-model-engine-builder-38ddc643` | Model raw engine-builder asm/codegen effects under the hard hook so the generated `0 set-check` boundary can be removed. |

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

This review was read-only. No gates were run by the subagents. The latest fully
validated committed port stack after the F08 prefix-list batch passed:

- `trust-lint`: 236 TRUST sites, 318 manifest rows, 0 findings;
- `tools/bootstrap-codegen-test.f`: `test: ok`,
  `bootstrap-codegen-test: ok`;
- `bin/hb test/engine-suite.f`: `ok`;
- focused `build-helper-fixtures` bundle:
  `build-fixpoint-test: ok`, `hb-build-test: ok`, `codesign-test: ok`;
- `test/run.f`: `PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build)`.
- recovery-host probe: installed `gforth 0.7.3` failed the required `{:` locals
  probe with rc 1, so `tools/bootstrap.sh` exited 69 before generation and left
  `bin/hb` checksum
  `d2d79b59c70a4de0d160b886ededde6941a92feb29bef3b105d26800b1d3793b`
  unchanged.

## Agent Command Notes

Agents used `rg`, `sed`, `nl`, `wc`, and definition-length scans to locate large
or uncommented definitions and duplicated command/process patterns. One tools
scope used a one-off Python definition-length scanner during read-only analysis;
no host-language code was added to the repository.
