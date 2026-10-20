---
title: Check kernel identity against emitted bytes
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T22:23:58.492006+03:00"
closed-at: "2026-10-20T22:36:55.021800+03:00"
close-reason: Removed only three unsupported cross-revision output-kernel equality assertions. Actual emitted-code hashing, direct/quoted primitive checks, producer invalidation, runtime/rejected input and identical-source cold/cached equality remain. Original full edit matrix passed those checks and failed only the removed early assertion. Verify-only passes; independent Astra review clear. No production hash normalization or descriptor machinery.
---

The edit matrix exposed an unsupported equality assertion between kernels from different source trees. Kernel identity hashes the finalized emitted bytes, including instructions addressing metadata after a variable-sized DATA payload. A source edit can move that metadata and correctly change the digest. Remove only cross-revision identity assumptions; retain each image's actual-code digest check, direct and quoted calls, changed-host invalidation, runtime values and refusals, and exact cached/cold roles, executables and maps for identical edited sources. Independent adversarial review must confirm the retained coverage and actual-byte contract.
