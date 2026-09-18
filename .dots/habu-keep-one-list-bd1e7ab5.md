---
title: Keep one list of the pre-checker definers
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T03:41:57.394947+03:00"
---

Problem: a definer written before src/core/checker.f loads needs its effect in src/core/cell-effects.f (else internal-mark.f seals it DNAME-INT: measured 2026-09-18, a verbatim clone of PTR-VARIABLE under another name is sealed while the original is not) and its published row in src/habu/verify-source.f's definer table (else every use fails the pre-scan), and tools/lint/def.f keeps a third copy of the same names that is already incomplete (PERSISTED-PTR-VARIABLE is absent; PTR-U8-TABLE and PERSISTED-PTR-U8-TABLE-VARIABLE were added to the first two only). Acceptance: one declared list of the pre-checker definers that the three consumers read (or a lint that refuses a definer present in one and absent in another), with tools/lint/def.f completed and a test that adds a definer to the list and sees all three surfaces agree. Files: src/core/cell-effects.f, src/habu/verify-source.f, tools/lint/def.f, test/. Verify: the test; tools/lint on the tree; test/run.f. Depends: none. Ownership: declared storage and lint. Claim: unassigned.
