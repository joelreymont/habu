---
title: "Assert a declaration's record is its only report"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T20:04:31.533599+01:00"
---

Problem: test/load-reject-diag-test.f ASSERT-DECL (in nuynnwzp, not on master, :805) promises a declaration's rejection is one record and no second report, but checks only that 'habu: bad' is absent; every rejected declaration still prints 'hb: uncaught throw code 71xx' after its record (src/habu/habu2.f LUNCAUGHT prints it for a code outside 1..255 before reading REJECTION-ABI:CODE-CELL), which jerry's 9fa40dba 'Exit a published rejection without the uncaught line' removes. Acceptance: ASSERT-DECL also asserts stderr has no 'uncaught throw' line, and TEST-DECL passes with 9fa40dba on master. Depends: 9fa40dba on master. Claim: unassigned.
