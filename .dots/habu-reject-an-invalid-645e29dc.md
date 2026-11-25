---
title: Reject an invalid role constant at its source line
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:42:08.330530+01:00"
---

Problem: a CAST: role maker takes any n, so `0 MEM:BYTES-ALLOC-LEN` (CAST: at src/lib/memory.f:102) makes an alloc-byte-len of 0 and a program doing it loads rc 0, though AS-ALLOC-BYTE-LEN (src/lib/num-types.f:102) rejects zero. The arms that threw for such values were deleted as unreachable from the library's own constants. AGENTS.md: no runtime checks, check statically. Acceptance: a role value written as a constant is checked against its role's rule when the program is checked, and `0 MEM:BYTES-ALLOC-LEN` is rejected at its line, rc 70. Depends: f54fea56, whose NEWTYPE clause LEAST states a role's least value. Claim: unassigned.
