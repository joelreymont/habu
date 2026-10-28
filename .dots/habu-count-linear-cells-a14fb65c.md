---
title: "Count linear cells in a word's return-row effect"
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:39:18.103221+02:00"
---

Problem: EFF-APPLY (src/core/checker.f:10382-10405) applies a called word's declared return rows rin/rout (RSUNI-IN, RCUR !) after RECORDED-STEP with no linear count check, and LIN-EFF-PASS checks only type-variable multiplicity, so a word effect whose return-row parts name a linear type is outside conservation. No library word declares one today. Acceptance: a call whose return rows create or drop a linear cell is refused like a data-row one (LIN-SNAPSHOT/LIN-CHECK across the whole step, :3673-3680), with a refusal row and a positive row that moves a token through the return stack unchanged. Files: src/core/checker.f, test/linear-suite.f. Verify: the rows. Depends: none. Ownership: type system. Claim: unassigned.
