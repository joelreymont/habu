---
title: Carry or scratch image-lifecycle state as one
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T13:29:30.004074+02:00"
---

Problem: src/lib/image-lifecycle.f:11-12 keeps the hook count N as a plain variable, which file objects carry, while HOOKS is a DYNAMIC-BUFFER whose #base/#ctl are scratch cells (src/core/layout-buffer.f DBUF-MARKS) that a link writes zero. The owners' registered flags are also plain and carried: build-out.f:32 CLEANUP-LIVE, engine-id.f:20 EID-CLEANUP-LIVE, task.f:454 SEM-CHUNKS-ARMED, task.f:1266 SWEEP-ARMED, context.f:504 HOOKED. A file that registers a process hook at load time would link a nonzero N against an empty HOOKS, and PREPARE would index past it (E-LAYOUT-BOUNDS), as ffi-abi.f's retired spans did (fixed in carl-c5a2 by moving them into 64-slot TYPED-BUFFERs). Harmless today: N is 0 when hb's build reaches end.f. Acceptance: every count and its table, and every flag and the registration it records, are both carried or both scratch; a warm build after an edit stays byte-equal to cold. Files: src/lib/image-lifecycle.f and the five owners above. Verify: the edit measure (cold base, sort.f edit warm, cold of edited tree; cmp hb and hb.map). Depends: none. Ownership: those files. Claim: unassigned. Workspace: none yet.
