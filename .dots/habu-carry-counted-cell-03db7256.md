---
title: Carry counted cell runs as one item
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T13:19:15.427653+02:00"
---

Problem: about 399 signatures pass a non-byte pointer and a count as two stack items (295 ptr n n, 95 ptr a n, 9 others), the two-item shape the string work removes for bytes; how many are counts is unmeasured, and the core combinators such as EACH and FOLD load before src/lib/span.f. Acceptance: a design the user approves for runs of cells, then the tree on it. Depends: habu-spell-pointers-t-9f752b95. Claim: unassigned.
