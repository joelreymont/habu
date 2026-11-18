---
title: Trace the completion checks the cursor rewrite dropped
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T21:52:26.094028+01:00"
---

Problem: soluwtst (test/checker-completion.f, jerry-dbp) cut master's checks from 84 to 39. Line 621 (CMQA:CmqB: PART-PATH) is rejected E-BAD-QUALIFIED-TOP-LEVEL with no candidate call, so it cannot fail. Six failure modes left the file: an arm outliving its check after a rejection or a throw, a nested arm or enumeration, the observer changing the verdict, uses or store, an engine word the body has not used yet, a rejected redeclaration moving or renaming a word, a neutral scope's rewind. Acceptance: line 621 is gone, and each mode is checked through the cursor or by a named kept test that fails without it. Depends: none. Claim: unassigned.
