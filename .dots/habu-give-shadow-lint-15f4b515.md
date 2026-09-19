---
title: Give shadow-lint ADD-PRIM a labelled capacity exit
status: active
priority: 3
issue-type: task
created-at: "2026-09-17T19:56:10.897429+03:00"
---

Problem: tools/lint/shadow-lint.f ADD-PRIM has no capacity guard of its own; before habu-convert-the-raw-8052992f it silently wrote past POFF into PRIM-LEN beyond PMAX, and after it (POFF is a declared TYPED-BUFFER) it throws the bare E-LAYOUT-BOUNDS instead, a strictly safer but unlabelled exit unlike its siblings C-ADD and TOKEN-ENSURE. Acceptance: ADD-PRIM refuses past PMAX with the lint's own labelled diagnostic naming the count and the ceiling, with a test that fills the table. Files: tools/lint/shadow-lint.f, tools/lint/ tests. Verify: the test; tools/lint/shadow-lint.f on the tree. Depends: habu-convert-the-raw-8052992f landing. Ownership: lint. Claim: alder; workspace .jj-ws/alder-shadow-capacity from b4efad25.

The same ADD-PRIM copy also lacks a bound on PNAMES. Guard both stores before changing bytes, rows, or counts, using a labelled capacity refusal.

Ready for Hazel review/integration: ADD-PRIM now checks row and name-byte capacity before copying or publishing a row. Exact limits remain valid. Both failures use E-SHADOW-CAPACITY and name the store, current count and ceiling.

Validation: the whole shadow-lint registry row passes at tiers 0 and 1 on a private host/tree. Fixtures fill 512 rows and 8192 name bytes, then check the refusal diagnostic and unchanged state; oversized and negative name lengths are refused too. Removing the row guard makes three new assertions fail (generic bounds code, missing label, copied bytes). Production scans 140 prims clean. Global error-code lint: 1097 files, 723 claims, 57 reservations, zero findings. Independent Astra review clear. No engine change or full gate.
