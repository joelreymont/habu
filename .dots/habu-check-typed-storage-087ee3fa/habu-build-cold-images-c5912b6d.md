---
title: Build cold images with typed stores
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T12:53:45.212043+03:00"
closed-at: "2026-10-19T13:35:22.208227+03:00"
close-reason: Capture offsets are explicitly numeric via the public byte-view subtraction idiom. Exact matching native AOT WID and chain, affected capture/native/Wasm consumers pass; independent Astra review clear. Typed storage checks remain unchanged.
---

Matching privatea7c8b6f0 engine fails the required HABU_AOT_GATE=1 test/aot-wid-build.f after1964compiled definitions with top-row ! typed effect mismatch. Reduce the exact cold builder source/selected binding; repair the responsible Habu declaration/checker/loader boundary while preserving the newly requested typed top-level storage rule. Existing failed cold-path E2E is the regression; independent review and matching native check required.
