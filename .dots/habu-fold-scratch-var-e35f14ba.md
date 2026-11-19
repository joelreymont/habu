---
title: Fold SCRATCH-VARIABLE and SCRATCH-BUFFER into the clause
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:35:20.233406+01:00"
---

Problem: SCRATCH-VARIABLE (src/core/pointer-storage.f:14, a raw `^a` cell in scratch) and SCRATCH-BUFFER: (src/lib/string.f:44, `^u8` bytes in scratch) say a second way what a typed definer with the clause says, and they say it with no type. Acceptance: both are deleted and every caller (about 30 files) declares its scratch storage with TYPED-VARIABLE or TYPED-BUFFER, its type, and SCRATCH-ONLY. Tender calls SCRATCH-VARIABLE (tender src/probe.f:57-62, src/blank.f:86-87), so the deletion moves a consumer; SCRATCH-BUFFER: holds bytes, which TYPED-BUFFER rejects (docs/forth-card.md:411), so a byte scratch buffer needs another typed form before this acceptance can hold. Depends: none. Claim: unassigned.
