---
title: Typed-storage definers refuse a taken or bad name with no message
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:21:44.213514+02:00"
---

Problem: under --load a duplicate typed-storage or heap name exits 78 with no message (src/core/layout-buffer.f:68-69; cases e/dup-tvar, dup-heap, dup-qual-*, dup-base-tvar, where the check prints nothing either); `TYPED-VARIABLE field-project n` exits 78 silently in both modes; an ARENA or OPAQUE structure refused for a duplicate or taken name prints at 'pt' with no file and uncaught throw code 7102 or 7110 (e/dup-arena-fam, dup-opaque-fam, dup-opaque-make). Outputs in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/sealdef/out/e-*; hunt /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/sealdef-hunt.md (Out of scope). Related: habu-locate-storage-syntax-4eb9fae5 (two other storage faults with no diagnostic). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names each silent exit as path:line; then each case prints a diagnostic naming the declared token and its place under the load and the check, and tests pin them. Files: src/core/layout-buffer.f, src/core/heap.f, src/core/arena.f, src/core/sumtype.f. Verify: the e/ cases. Depends: habu-a-typed-storage-f57ce21c (same definers' name checks). Ownership: the typed-storage definers. Claim: unassigned.
