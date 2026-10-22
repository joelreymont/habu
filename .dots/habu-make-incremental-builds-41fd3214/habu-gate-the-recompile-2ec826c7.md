---
title: Gate the recompile count after small edits
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T12:17:10.199700+03:00"
---

Problem: no gate row measures recompilation after a small edit. test/compiler/native-build-edit-e2e.f is in no gate (docs/gate.md:28-33 optional), and its COUNTS (:437-450) asserts only hits>0 and compiled>0, which every measured regression passes. DIAG-CASE in test/native-file-auto-e2e.f:369-370 checks refusals are absent with HABU_NATIVE_CACHE_DIAG off, so it can never fail. Acceptance: one gate row with a persistent private cache and HABU_NATIVE_CACHE_DIAG=1 on a realistic multi-file fixture (several packages per file, a type declaration, TRUSTED:, forward verification), through hb-build and once hb-build --engine: no edit -> artifact hit, compiled 0; comment-only edit -> compiled 0, no cache-store-refused for colon definitions, no dependency miss outside the file; body edit of one definition -> compiled = 1 + its real dependents, product runs and equals a fresh-cache build. Delete COUNTS and the vacuous DIAG-CASE checks. Files: new gate row, test/gate-*.f, docs/gate.md. Depends: the two cache-defect dots (row lands with them or after). Claim: unassigned.
