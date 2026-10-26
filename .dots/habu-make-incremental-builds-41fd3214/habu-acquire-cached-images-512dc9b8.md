---
title: Acquire cached images before use
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:57.304779+02:00"
---

From the external plan W9 (I4), after the stack. The keyed-image prune race is documented at src/lib/build-cache.f:38: a support image or product member can be pruned between lookup and use. Acquire into the held work directory first, validate the acquired pair, treat disappearance as a miss, keep real I/O errors. No leases, global lock or retry machinery. Acceptance: an e2e that prunes between lookup and use rebuilds instead of failing. Depends: stack landed. Claim: unassigned.
