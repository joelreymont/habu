---
title: Key support images on observed child inputs
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:57.266248+02:00"
---

From the external incremental-build plan W1 (I1), Astra-checked 2026-10-26, after the stack. The AOT/REPL support paths key support images on a parent-side predicted closure scan (tools/hb-build-lib.f:574); the engine path already derives identity from the child's observed inputs (tools/native-build-image.f:42). Extend that existing mechanism: the child returns consumed source digests, queries and environment observations; publish the key after the result. Delete the parent closure scan/prehash once callers migrate. Acceptance: producer-race e2e where the child reads B while the parent sees A publishes no B image under A; unchanged rerun reuses. No task graph. Depends: stack landed. Claim: unassigned.
