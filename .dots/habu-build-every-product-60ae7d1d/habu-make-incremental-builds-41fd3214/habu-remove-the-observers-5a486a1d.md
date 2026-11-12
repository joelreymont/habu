---
title: Remove the observers and exports only the old builder used
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T08:30:48.373720+02:00"
---

Problem: the old builder (tools/build-core.f) is deleted, and these words had no caller but it. (1) src/habu/interpreter.f OBSERVE-TICK, OBSERVE-WORD (src/habu/linker.f:5049 passes it as an op), OBSERVER, OWNED?, OBSERVE-ADOPT/-ARM/-RESTORE/-BEFORE, AFTER-RUN, OBSERVE-FINISH (:517-610) with the source-exec-run primitive (habu1.f, prims.f, kernel-x64.f, SOURCE-EXEC-CTX-CELL). (2) src/compiler/native/host.f RUN-OBSERVE, OBSERVE-RUNS, UNOBSERVE-RUNS (:413-441) and the reason classes' admission; src/compiler/native/publish.f UNIT-OBSERVER, WITH-UNIT, PUBLISH-UNIT. (3) src/lib/ffi-abi.f:1536 PENDING. (4) src/lib/build-input.f: USE has no caller, so FILE (src/lib/browser/app.f:37, src/lib/browser/host.f:29) does nothing. Acceptance: each word above is removed with its cells and primitive rows, or kept with a caller named in its comment; engine cold and warm builds agree (0 objects rewritten, hb equal before the signature); the suite passes. habu1.f edits follow docs/design.md "Machinery being replaced" (never extend it; removing a primitive row is allowed). Depends: none. Claim: unassigned.
