---
title: Preserve CATCH data values
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T20:24:49.935728+02:00\""
---

Full context: src/habu/habu1.f BCATCH/BTHROW restore only XDS on THROW. Because the data stack is full-descending, an xt can pop an input and push another value into the same cell before throwing; pointer restoration then exposes the overwritten value instead of CATCH's original i*x. EVALUATE stack snapshots cannot own or fully fix this: Catch owns data/return/loop/handler/machine stacks, and direct throws never enter EVALUATE. Extend the CATCH frame with an exact live data-stack value checkpoint, restore bytes plus XDS only on exceptional return, guard checkpoint mappings from ordinary stores, preserve normal return semantics, mirror bootstrap codegen, and add direct/evaluated/nested overwrite regressions plus allocation/protection/performance tests. Integrate with habu-restore-catch-return-1074ce3f; remove evaluator-owned GSTACK copying once CATCH proves ownership.
