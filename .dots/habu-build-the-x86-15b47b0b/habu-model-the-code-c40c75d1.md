---
title: Model the code-provenance band on x86
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T09:22:47.271870+03:00"
blocks:
  - habu-emit-x86-code-973a0074
---

Problem: `code-origin` has x86 consumers that must answer truthfully: `snap-lib.f:453` (retained code without native evidence gives rc 100), `tools/native-build.f:9` (the self-build driver, G3) and `test/tier.f:366-371`. Dropping provenance on a tier-1-only engine would report `patch32`-written bytes as native, the exact hole `habu1.f:2417-2419` closes. Decision (K-lane design, 2026-09-30): model the band on x86.
Acceptance: `src/habu/code-origin-x64.f`, package `X64PROV`: twins of `code-origin.f` `EMIT-SET`/`EMIT-QUERY` (registered `engine-code-origin-set`/`engine-code-origin-query`) and `INVALIDATE,`/`UNKNOWN-RANGE,`/`NATIVE-RANGE,`/`QUERY,`/`OPEN,`/`CLOSE,` over the same DATA layout (`layout.f:1775-1785`). Add the calls into K8b's `patch32` and `code-publish` and K9a's `code-origin` row. Cases through the booted harness: a published span answers native, a `patch32` inside it answers unknown, an untouched address answers unknown, each with a negative twin.
Note for lane I: every `TIER-PROV:OPEN,`/`CLOSE,` call site is the assembly interpreter (`habu2.f:2783,7704,9259-9381`), so I5e adds provenance open/close rows to `prims.f` with bodies on both targets; this leaf provides the x86 bodies.
Files: `src/habu/code-origin-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-kernel-engine.f`, `docs/x86-64.md`.
Verify: host K3's product engine: `--load test/x86-64-kernel-engine.f`; ThinkPad: the images natively.
Depends: habu-emit-x86-code-973a0074 (K8b), habu-emit-x86-engine-86b5f8e7 (K9a).
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.
