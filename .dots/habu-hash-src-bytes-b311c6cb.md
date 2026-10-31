---
title: Hash source bytes once and skip object readback
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:57.293936+02:00"
---

From the external plan W7 (I6), after the stack. Source bytes are hashed twice across loader handover (tools/native-build-core.f:196 and tools/native-definition-cache.f:1153), and each new file object is written, reread and decoded (native-definition-cache.f ~1426). Give source-byte observation one owner; validate persisted objects when acquired, not right after writing. Acceptance: warm and cold engine build wall time measured before/after; e2e unchanged. Cut ancestry interning and new indexes unless measured. Depends: stack landed. Claim: unassigned.
