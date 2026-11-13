---
title: A stripped program runs with the store guard open
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T17:11:58.082469+02:00"
---

Problem: in a stripped program a store into CUR-CELL, WIDN-CELL and the package cells succeeds (rc 0), where the engine refuses it with exit 83. Under the installed hb's --load, `data-base CUR-CELL + @ data-base CUR-CELL + !` and the same for WIDN-CELL, at top level or inside a colon definition, run: a line typed after each prints and the load exits 0. Not established: whether a stripped program is meant to run unguarded (no checker runs in it) or the seal is lost in the strip. Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: where the guard is armed (no word named SEAL-FRIEND is left in the tree) and why a stripped program's start does not arm it, as path:line; what docs/design.md says a stripped program guarantees. Acceptance: named by the hunt. Depends: none. Claim: unassigned.
