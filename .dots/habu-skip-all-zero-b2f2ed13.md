---
title: Skip all-zero bitmap groups in the captured DATA encoding
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T15:10:00.374600+03:00"
---

Problem: the cell bitmap b9f1f792 writes for captured DATA covers the whole 8,346,672-byte span at one bit per cell, 130,548 image bytes (tools/engine-size.f row aot/data-cell-bitmap) although most 64-byte bitmap groups (4 KB of DATA each) are all zero; a presence bit per group costs about 255 bytes and lets the writer drop the zero groups, so the bitmap shrinks toward the groups that hold the 839,616 bytes of values. Acceptance: a second-level presence map over 64-byte bitmap groups, written at capture and read by the stripped startup (src/habu/aot-lib.f EMIT-DATA-COPY, pinned by test/gate-aot-image.f CHECK-COPY so the pin moves with it); one encoding stays (no per-page hybrid); engine size before and after with tools/engine-size.f; stripped boot median unchanged within noise (120 runs); byte fixpoint; full gate; docs/engine-size.md numbers updated. Files: src/habu/aot-decl.f, src/habu/aot-capture.f, src/habu/aot-lib.f, tools/engine-size.f, test/gate-aot-image.f, docs/engine-size.md. Verify: bin/hb --load tools/engine-size.f; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: capture format (hazel). Claim: unassigned.
