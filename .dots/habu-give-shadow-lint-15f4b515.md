---
title: Give shadow-lint ADD-PRIM a labelled capacity exit
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T19:56:10.897429+03:00"
---

Problem: tools/lint/shadow-lint.f ADD-PRIM has no capacity guard of its own; before habu-convert-the-raw-8052992f it silently wrote past POFF into PRIM-LEN beyond PMAX, and after it (POFF is a declared TYPED-BUFFER) it throws the bare E-LAYOUT-BOUNDS instead, a strictly safer but unlabelled exit unlike its siblings C-ADD and TOKEN-ENSURE. Acceptance: ADD-PRIM refuses past PMAX with the lint's own labelled diagnostic naming the count and the ceiling, with a test that fills the table. Files: tools/lint/shadow-lint.f, tools/lint/ tests. Verify: the test; tools/lint/shadow-lint.f on the tree. Depends: habu-convert-the-raw-8052992f landing. Ownership: lint. Claim: unassigned.
