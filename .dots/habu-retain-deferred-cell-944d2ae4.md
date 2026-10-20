---
title: Retain deferred-cell origins during reuse
status: active
priority: 1
issue-type: task
created-at: "2026-10-16T14:05:30.279860+03:00"
---

A warm self-build has 157 address-capture refusals because DO-IS emits a named deferred dispatch-cell address with unknown origin. Carry the exact selected declaration through the existing HIR origin path; resolve its checked DEFER-MAGIC trailer by record identity during capture/import. Preserve existing DATA references and pointer refusals. Explicitly record the consumed full contract, including deferred class, so same-key variable/colon replacement cannot reuse an installer. Extend existing real session replay E2E before code with private/global decoys, moved cells, class changes and retirement/redefinition; verify actual hits and runnable target behavior, independent Astra review, matching engine/native gate and convergence.
