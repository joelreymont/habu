---
title: Fill NEMIT from X64EMIT and publish x86 emissions
status: closed
priority: 2
issue-type: task
closed-at: "2026-09-30T15:32:06.018534+03:00"
close-reason: "done: x86 emit fills NEMIT rows and RETIRE clears them; spark chain from 613a converges at 39723d8c (gen1-5), gate 501 of 501 rc 0; ThinkPad x86 proof with it: 17 suites ok incl. code-span, 107 images unchanged, manifest bad=0."
created-at: "2026-09-29T12:51:36.509382+03:00"
---

Problem: after P1 only the A64EMIT adapter fills `NEMIT`, so `NPUB` cannot publish an x86 emission; the x86 engine publishes its own tier-1 definitions at runtime.
Acceptance: `src/arch/x86-64/passes.f` fills the surface from C6's site rows; publish records x86 rows through the P2 band; a host-side unit publishes an x86 module into a scratch region under an x86 binding; refusal atomicity (wrong target, out-of-range rel32, retired buffer) leaves `cp@`, `ndict@` and the band unchanged.
Files: `src/arch/x86-64/passes.f`, `src/compiler/native/publish.f`, `test/compiler/x64-publish.f`.
Verify: spark `bin/hb --load test/compiler/x64-publish.f` and the focused publish suites; rebuild and gate (`publish.f` is in the engine).
Depends: habu-add-the-nemit-201c6fbf (P1), habu-represent-x86-live-729a7ac6 (P2), habu-record-symbolic-x86-10037f07 (C6).
Route: Alder (shared: src/compiler/native/publish.f).
Ownership: krait (Intel lane).
Claim: krait.
Preflight note from P1: `src/habu/code-span.f` assumes 4-byte dictionary spans (`CODE-SPAN:EXACT` dies on size mod 4, lines 11, 17-18, 32-34); this leaf owns making it byte-granular for x86 spans.
C6 landing note: `X64EMIT` has `FUNCTION-OFFSET@` but no function-count reader (the ARM64 adapter loops `A64EMIT:FUNS`, `src/arch/arm64/passes.f` FUNCTION-ROWS); P3 adds that one-line reader in `src/compiler/native/emit-x64.f` (add it to Files). Call rows name the instruction's first byte; the rel32 field is at +1.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Host scope: the x86 emit row fills and retires `NEMIT`, and exact spans become byte-granular. Nothing commits on ARM64: NPUB writes only through the live engine's rows (`publish.f:20-39`), which take whole words there (`habu1.f:2394-2402`), so the scratch-region unit is dropped. `publish.f` is unchanged: it hands each site's first byte to `callmap-set`/`addrmap-set`, which write P2's band on x86 (`kernel-x64.f:2074-2075`). Native proof: X6.
- `emit-x64.f`: `X64EMIT:FUNS ( -- n )`, sealed only (refuses `E-X64EMIT-STATE` unless sealed), beside `FUNCTION-OFFSET@` (`:1340`).
- `src/arch/x86-64/passes.f`: `EMIT` ends `at ROWS`; `ROWS ( n -- )` is `X64EMIT:BYTES X64EMIT:SIZE 0 NEMIT:OPEN`, `at NEMIT:PLACE`, `FUNCTION+` per `FUNS`, `CALL-SITE+` per call row, `ADDR-SITE+` per address row, `SEAL`. `: RETIRE ( -- ) X64EMIT:RETIRE NEMIT:CLEAR ;` replaces the row at `:275` (twin: `arm64/passes.f:298-301`).
- `CODE-SPAN`: an exact span is any byte count 1..`MASK` on both targets (`SIZE?`, `VALID?`'s FULL arm); a legacy span stays whole ARM64 words. `aot-decl.f` `SPAN>Q` (`:630-632`) packs 4-byte units, so it refuses `BODY` mod 4 <> 0 itself rather than truncate.
- "Wrong target" is dropped here: `NEMIT` names no machine and the engine reads none before K12; X1 takes it. Out-of-range rel32 is `E-X64EMIT-REACH` inside `EMIT`; `NEMIT` stays unsealed.
- `NPUB:NEXT-SLOT` stays `cp@`: K8c (`habu-keep-the-x86-6967d3cf`) keeps x86 CP on 16-byte slots, so NPUB learns no unit.
- Tests, red first, extend `test/compiler/x64-chain-fixture.f` (`SUITE compiler-x64-chain`); no `x64-publish.f`. Read off `NEMIT` after `NBACK:EMIT`: the pinned DIFF image, `RET-BYTES` 0, `PLACEMENT` = `EMIT-SLOT`; `PCALLER`'s row (`CALL`, `$400`, at an `$E8` byte); a quoter (`x64-emit-fixture.f:631-646`): two function rows, one `ADDR-CODE` row (if the quoter does not pass `NBACK:SELECT` under `X64PASS`'s two-function routine, drop that case and leave function and address rows to X6). A callee 2^32 away: `E-X64EMIT-REACH`, then `NEMIT:SIZE` throws `E-NEMIT-STATE`. After `RETIRE`, `NPUB:PUBLISH-PENDING` throws `E-NEMIT-STATE`, `cp@`/`ndict@` unchanged. `test/compiler/code-span.f`: `$80000003 VALID?` true, `3 VALID?` false, `3 SIZE?` and `MASK SIZE?` true, `17 EXACT BYTES` 17. Pre-change both fail (`E-NEMIT-STATE` -8564; the pins).
- Files: `src/compiler/native/emit-x64.f`, `src/arch/x86-64/passes.f`, `src/habu/code-span.f`, `src/habu/aot-decl.f`, `test/compiler/x64-chain-fixture.f`, `test/compiler/code-span.f`, the headers saying no row fills `NEMIT` (`passes.f:22-30`, `emit-x64.f:57-61`, fixture `:17-25`), `docs/x86-64.md` "The pass chain" (`:389-430`). Route: Alder (`code-span.f`, `aot-decl.f`).
- Verify (spark): `compiler-x64-chain`, `compiler-x64-emit`, `compiler-code-span-capture`, `compiler-native-code-span`, `stripped-does`; rebuild (`code-span.f` is baked, `native-runtime.f:89`); chain; gate.
