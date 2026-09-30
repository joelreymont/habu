---
title: Port the profiler tick and alternate stack
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.880210+03:00"
closed-at: "2026-09-30T17:16:53.884826+03:00"
close-reason: "(1) 49f8dc6f: ARM64 engine and .names byte-identical, fixpoint refresh, bootstrap check-only and clobber-lint on spark; (2) hb-x64-kernel-prof images exit 0, 21 and 78 on the ThinkPad (76 at ENTRY-LABEL before), the full x86 proof plain and shimmed, the suite ok on spark"
blocks:
  - habu-share-one-x86-0a045dc8
---

Problem: the profiler's sampling handler models aarch64 frames only (`src/habu/prof.f:47-50` refuses other targets; `prof.f:867-918` is the handler and alternate-stack contract).
Acceptance: an x86 sampling handler reading `RIP`, `sigaltstack`, a refused mapping named and fatal (`prof.f:867-918` semantics); the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: a routine image taking profiler ticks on the alternate stack.
Depends: habu-port-signals-crash-2c7768ca (K10a).
Route: direct.
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Scope: the sampling half of `src/habu/prof.f` (`286-325`, `798-845`, `849-932`) and rows `prof-on`, `prof-off`, `prof-reset`, `prof-rate`, `prof-pc>rec`. `prof-report`, `prof-json`, `prof-row`, sync and the limit report are K10e's (`habu-port-the-profiler-c77ee1af`); `KERNEL,` registers nothing for them meanwhile: only `habu2.f:11170` runs `ENGINE-PRIMS:COMPLETE`, and X5 depends on K10e.
- Tick: `EMIT-PROF` without the limit test. Saved rbp DATA-VA and r13 `PROF-DBASE` make a Habu sample, else `PROF-FOREIGN`. No arena: `PROF-OTHER`. Rip indexed: its counter, inclusive once, walk. Rip at or above `ARN-HI`: defer rip and the cell at the saved rsp, walk. Else `PROF-OTHER`, walk. The walk is `C-PROF-WALK` without x30, each code cell searched at cell-1. Then `PROF-TOT`+1, `ret` into its own `RESTORER,`.
- Band owner: new build-only `src/habu/prof-abi.f`, package `PROF-ABI`: `prof.f:56-149` under their names except `MACOS-SA-PROF-FLAGS`; texts `PROFMMAPMSG$` `PROFSTKMSG$` `PROFARNMSG$` (newline included) replace the `-LEN`s; `PROF-MAP-RC` 78, `PROF-LIMIT-RC` 99; `PROF-BAND-AT ( n n -- n )`, DATA base and size to band. `prof.f` requires and uses it; ARM64 engine byte-identical. Not `layout.f`: baked (`native-runtime.f:55`). `tools/build-fixpoint.f:1100` appends it as a module first (precedent: `src/habu/data-bands.f:1-12`).
- x86 owner: new `src/habu/prof-x64.f`, package `X64PROF`, over X64BOOT's signal words and habu-share-one-x86-0a045dc8's layout package (no fourth replay; Depends add it; that package must load on its own before `boot-x64.f` and publish `DATA-VA` and `DATA-SIZE`). `kernel-x64.f` requires it; `PROFILER,`, last in `KERNEL,`, calls `X64PROF:HELPERS,` and registers `ON-BODY` … `PCREC-BODY`. `docs/x86-64.md` "Profiler rows" states the find, edge and index contracts K10e reuses.
- Test: `test/x86-64-kernel-prof.f`, package `X64K-PROF`, `SUITE x86-64-kernel-prof` after `x86-64-kernel-ffi`; harness `CODE-RECORD, ( ptr u8 n label label -- )`. `hb-x64-kernel-prof` exits 0: bounded spins reach a record's counter and its caller's inclusive count with rsp 64 bytes above the DO/LOOP stack's guard (a frame pushed there dies 139), `PROF-OTHER` below the index, `ARN-DEFER` above, `PROF-FOREIGN` with rbp moved; the identity `prof.f:798-802`; `prof-pc>rec`, `-reset`, `-rate`, `-off`. `-negative` 21. `hb-x64-kernel-prof-refused`: `prof-rate`, RLIMIT_AS 0 (setrlimit 160), `prof-on`: fd 2 `hb: prof-on: cannot map the handler stack`, 78. Pre-change: `ENTRY-LABEL` dies 76 on `prof-on`.
- Two commits: (1) "Share the profiler band layout" (`prof-abi.f`, `prof.f`, `build-fixpoint.f`); measured violation: `rg -n 'constant PROF-TOT' src/` finds only `prof.f:56`, private to `PROF`. Spark: this tree's ARM64 engine and `.names`, built by the base engine, `cmp` equal to the base's; `HABU_FIXPOINT_ENGINE=<private copy> bin/hb --load tools/build-fixpoint-refresh.f -- all --force`; `HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh` (prof.f gains a `require`); the `clobber-lint` suite. (2) "Port the profiler tick to x86-64", after habu-share-one lands; full x86 proof (`KERNEL,` grows in every booted image).
- Files: drop `boot-x64.f`, `x86-64-peer-routines.f`; add the above, `docs/porting.md:128-134`. Route: Alder (shared `prof.f`, `build-fixpoint.f`).
