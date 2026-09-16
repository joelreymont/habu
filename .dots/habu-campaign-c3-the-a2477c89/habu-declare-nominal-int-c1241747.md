---
title: Declare nominal integer types in source
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:54.708216+03:00"
---

Problem: nominal integer roles (CC-IDX, CC-LEN, CC-FD, CC-PID, ...) are constants in src/core/checker.f, so application code cannot declare its own distinct integer and falls back to bare n (MISSING.md Foundation A1). Acceptance: a source declaration form (MISSING.md sketches 'nominal frame-idx : n') registers a tag in a runtime-extensible table without an engine edit, generates the explicit converter pair, is as strict as the built-in roles (distinct from n and from every other nominal, no implicit collapse), and CON-OK? consults the table; positive and negative fixtures; the built-in roles are re-expressed through the same table; the explicit conversion count in Tender's sources drops after adoption (measure before and after with rg). Files: src/core/checker.f role table, the declaration word, docs/forth.md, docs/type-system.md, fixtures under test/. Verify: checker fixtures, byte fixpoint, test/run.f green, the Tender count. Depends: none. Ownership: checker lane. Claim: unassigned.
