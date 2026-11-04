---
title: Preserve cold literal layout during reuse
status: closed
priority: 1
issue-type: task
created-at: "2026-10-16T13:45:48.575160+03:00"
closed-at: "2026-10-31T20:52:54.194587+02:00"
close-reason: "Landed in lane 11 4b54ed21: POOL-PLAN places literals as cold; warm engine == cold byte for byte. Prefix-edit equality is rechecked when 1209a534 lands."
---

Same final source and inputs must produce identical cold and warm engine bytes and maps across cache histories, including earlier-prefix edits. Existing capture assigns literal IDs in emitted instruction order instead of actual NSTR interning order; KEY-NAME demonstrates reordered a64.imm and a64.tail-out allocations and changed MOVK immediates. Track actual interning dependencies at their owner, preserve compiler address identity and reject only genuinely changed dependencies. Inspect existing real app and engine cold/warm/edit E2Es before implementation; verify runnable artifacts, equality after prefix and segment-sharing changes, independent Astra review, full native suite and isolated convergence.
