---
title: Drop the primitives and rows tier 1 needed
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T17:57:04.287024+01:00"
---

Problem: SOURCE-PRODUCER:KERNEL$ lost its only caller with the host registry and was the only reader of the kernel-producer$ primitive; src/habu/prims.f keeps global trusted-only rows whose stated reason is the optimizing compiler's call window; E-HIR-UNMODELED (src/lib/error.f) is defined and never thrown; the compile latch still takes 2, the tier era's value. Acceptance: what nothing reads is deleted, what stays states today's reason, and the engine builds cold and warm-equal. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-t1n.
