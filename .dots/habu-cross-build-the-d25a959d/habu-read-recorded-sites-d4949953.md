---
title: Read recorded sites in the capture
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.983183+03:00"
blocks:
  - habu-represent-x86-live-729a7ac6
---

Problem: `src/habu/aot-capture.f:1-14,62-80,1673-1684` discovers calls by decoding `BL` (`ACAP-CALL?`, `ACAP-TGT`, `ACAP-ZERO-IMM`, `ACAP-SCAN-CALLS`) and reads address chains as MOVZ/MOVK.
Acceptance: `ACAP-SCAN-CALLS` and the chain scan are replaced by `SITES:EACH-IN-SPAN` over the window (ARM64 arm: the bitmap bits, decoding only the value at a recorded site); target identity through `ACAP-TGT>REC` as today; the `.names` sidecar and captured bytes identical for the same window (chain gen 5 byte-identical); the tier-0 window of the stdin route captures identically (both compilers record the bits: `habu2.f:658-676,6838`).
Files: `src/habu/aot-capture.f`, `src/habu/sites.f`.
Verify: spark: rebuild; chain to gen 5 byte-identical; the stdin-route capture compared; gate.
Depends: habu-represent-x86-live-729a7ac6 (P2). Serialise with X2b and I7 on `aot-capture.f`.
Route: Alder (shared: src/habu/aot-capture.f, src/habu/sites.f).
Ownership: krait (Intel lane).
Claim: unassigned.
Preflight note from P2: the ARM64 bitmap arm of `SITES:EACH-IN-SPAN` yields region-to-text calls only (`habu2.f:659-668`, `5565-5567`), while `aot-capture.f:1679-1690` (`ACAP-SITE-HERE`/`ACAP-BRANCH-HERE`) also resolves in-region, out-of-window calls and B branches by name. Replacing `ACAP-SCAN-CALLS` with the bitmap arm must account for those before claiming a byte-identical capture.
