---
title: Fold SCRATCH-VARIABLE and SCRATCH-BUFFER into the clause
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:35:20.233406+01:00"
---

Problem: SCRATCH-VARIABLE (src/core/pointer-storage.f:14, a raw `^a` cell in scratch) and SCRATCH-BUFFER: (src/lib/string.f:44, `^u8` bytes in scratch) say a second way what a typed definer with the clause says, and they say it with no type. Acceptance: both are deleted and every caller (about 30 files) declares its scratch storage with TYPED-VARIABLE or TYPED-BUFFER, its type, and SCRATCH-ONLY. Depends: none. Claim: unassigned.
