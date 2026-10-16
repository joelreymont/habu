---
title: Keep hashmap probe cursors per invocation
status: closed
priority: 1
issue-type: task
created-at: "2026-10-16T08:14:45.848465+03:00"
closed-at: "2026-10-16T09:55:11.009751+03:00"
close-reason: Use invocation-owned probe and clear state. Astra review clear; source hashmap suite and stripped native two-Task consumer pass with zero lost keys and 50000 used slots per table on qualified 600 engine.
---

On public 600eb464-4849828a two Tasks filling separate HM tables lose thousands of keys; sequential controls lose none. HM:PROBE and HM:CLEAR share SLOT/DONE/IX/TRIES package cursors. Make traversal invocation-owned using locals/stack while preserving table probing semantics; no locks for independent tables. State the storage class in hashmap.f and threads docs. Existing repro: /private/tmp/claude-501/-Users-joel-Work-kaizen/00cc00b7-1a50-453e-a4ea-3ae179fed951/scratchpad/hmrace/hmrace.f and hmrace-seq.f. Acceptance: both tables retain all 50000 keys, existing hashmap/caller coverage, independent review and qualified public source release.
