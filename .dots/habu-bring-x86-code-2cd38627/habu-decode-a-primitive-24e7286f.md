---
title: Decode a primitive past its inline data
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T21:24:18.759230+01:00"
---

Problem: CODE-DUMP:DECODE on x86-64 sha256-blocks prints 222 lines, then dies rc 85 (E-THROW -9511) where the primitive's constant table sits inside its record (src/habu/kernel-x64.f SHA256-BLOCKS-BODY), so the instructions after the table never print; other records with inline data may do the same. Acceptance: the dump steps over data a record marks and prints the whole primitive. Depends: none. Claim: unassigned.
