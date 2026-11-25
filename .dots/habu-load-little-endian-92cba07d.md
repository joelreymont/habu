---
title: Load little-endian words in one access
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:20:36.338412+01:00"
---

Problem: src/lib/le.f's U16@, U32@ and their stores access memory byte by byte (the 8-byte forms are one access), so a reader or writer of words runs 2 or 4 accesses where the machine has one. Acceptance: on a little-endian target each is one load or store of its width, measured faster in a warm build and no larger. Depends: none. Claim: unassigned.
