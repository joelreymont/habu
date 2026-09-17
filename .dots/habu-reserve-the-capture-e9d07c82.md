---
title: Reserve the capture row tables only when capturing
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T11:38:39.994584+03:00"
---

Problem: src/habu/aot-decl.f keeps AOT-SPAN:BUF (16384 rows x 8 bytes = 128 KiB, 27a38d14) and AOT-XTSITE:BUF (the same shape) as static `create ... allot` tables above the heap floor, so every engine's DATA heap carries 256 KiB of build-time capture rows that only a metabuild host ever fills; the file stays small because the DATA image is sparse, but the booted heap and the per-task copies pay for it (span lane, 2026-09-17). Acceptance: the capture tables become DYNAMIC-BUFFERs reserved by the capture (the REC-STORAGE pattern in aot-capture.f), sized by the record cap at capture time and absent from an engine that captures nothing; tools/engine-size.f and the boot heap high-water (HEAP-START-CELL and the run above it) measured before and after on a native-runtime engine; the AOT family and the whitebox suites green; byte fixpoint. Files: src/habu/aot-decl.f, aot-capture.f, habu2.f (readers of BUF@), tools/engine-size.f. Verify: engine-size; test/gate-aot-positive.f; aot-chain-capture-suite; fixpoint. Depends: none (27a38d14 landed). Ownership: AOT capture storage. Claim: unassigned.
