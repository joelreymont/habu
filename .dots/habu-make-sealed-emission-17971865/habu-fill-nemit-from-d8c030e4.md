---
title: Fill NEMIT from X64EMIT and publish x86 emissions
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.509382+03:00"
---

Problem: after P1 only the A64EMIT adapter fills `NEMIT`, so `NPUB` cannot publish an x86 emission; the x86 engine publishes its own tier-1 definitions at runtime.
Acceptance: `src/arch/x86-64/passes.f` fills the surface from C6's site rows; publish records x86 rows through the P2 band; a host-side unit publishes an x86 module into a scratch region under an x86 binding; refusal atomicity (wrong target, out-of-range rel32, retired buffer) leaves `cp@`, `ndict@` and the band unchanged.
Files: `src/arch/x86-64/passes.f`, `src/compiler/native/publish.f`, `test/compiler/x64-publish.f`.
Verify: spark `bin/hb --load test/compiler/x64-publish.f` and the focused publish suites; rebuild and gate (`publish.f` is in the engine).
Depends: habu-add-the-nemit-201c6fbf (P1), habu-represent-x86-live-729a7ac6 (P2), habu-record-symbolic-x86-10037f07 (C6).
Route: Alder (shared: src/compiler/native/publish.f).
Ownership: krait (Intel lane).
Claim: unassigned.
Preflight note from P1: `src/habu/code-span.f` assumes 4-byte dictionary spans (`CODE-SPAN:EXACT` dies on size mod 4, lines 11, 17-18, 32-34); this leaf owns making it byte-granular for x86 spans.
C6 landing note: `X64EMIT` has `FUNCTION-OFFSET@` but no function-count reader (the ARM64 adapter loops `A64EMIT:FUNS`, `src/arch/arm64/passes.f` FUNCTION-ROWS); P3 adds that one-line reader in `src/compiler/native/emit-x64.f` (add it to Files). Call rows name the instruction's first byte; the rel32 field is at +1.
