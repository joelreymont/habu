---
title: Decide REG-RESTORE dictionary coverage
status: open
priority: 3
issue-type: task
created-at: "2026-07-26T09:02:20.580063+02:00"
---

Problem: the test helper REG-RESTORE in test/enum-decl-suite.f rewinds registry cursors (family, variant, schema, field) but not the dictionary, so a suite block that generated constructor words leaves them defined after the restore. Making REG-RESTORE a complete reset would duplicate the dictionary participant of the production transaction inside a test helper - the ctor-participant delivery deferred this as a decision, not a leaf. Required result: decide once - either extend REG-RESTORE to also drop dictionary rows through the existing public dictionary-participant surface, or document at the helper that dictionary state deliberately survives and every block that generates words must use the production rollback path instead. Record the decision at the helper definition and align the suite blocks that currently straddle the gap. Acceptance: the suite passes with the decided semantics; a comment or an implementation at REG-RESTORE states the decision; no suite block depends on the undecided middle ground. Files: test/enum-decl-suite.f only. Verify: bin/hb --load test/enum-decl-suite.f. Depends: none. Ownership: the test helper semantics only. Claim: unassigned.
