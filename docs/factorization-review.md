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

## Remaining Work Order

These findings are still open as of this review artifact. The safest order is:

1. Add missing stack-effect comments in arch/platform/bootstrap files, because
   this improves checker-aided review without changing behavior.
2. Factor mirrored native/bootstrap numeric parser and colon-open paths together.
3. Factor bootstrap JIT duplicated binary prep and move-wide emission.
4. Factor byte cursor writers shared by ELF, Mach-O, and signing.
5. Factor process capture and gate progress helpers, then simplify child-spawn
   tests to true boundaries only.
6. Factor fs traversal, manifest-policy rows, stale-status scanner, and regex
   dispatch in separate changes.

## Verification Status

This review was read-only. No gates were run by the subagents. The validated
port stack before this report passed:

- `trust-lint`: 236 TRUST sites, 318 manifest rows, 0 findings;
- `test/gate-stdlib.f`: `PASS: native lint/stdlib gate phase`;
- `test/run.f`: `PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build)`.

## Agent Command Notes

Agents used `rg`, `sed`, `nl`, `wc`, and definition-length scans to locate large
or uncommented definitions and duplicated command/process patterns. One tools
scope used a one-off Python definition-length scanner during read-only analysis;
no host-language code was added to the repository.
