---
title: Make unreachable declaration codes engine defects
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T18:05:37.840707+01:00"
---

Problem: src/core/generated-declaration.f DECL-REJECT explains codes no declaration text reaches (E-PF-ID, E-PF-TX, E-PF-OWNER, E-PF-LAYOUT, E-PF-FLAGS, DECLARATION-TRANSACTION:E-FRAME, E-REPLAY-BUSY, E-CTOR-ARM, and the decl-event.f E-DEV-TX, -STATE, -FIELD-SCOPE, -FAMILY-SCOPE, E-SM-FAM rows) with the help 'report this declaration as a Habu defect', where the C4 design makes an engine defect a 76 die with a sentence at its throw; and the S-REASON and S-FIX slots copy their text into SPAN-CAP (96) bytes and cut a longer one, though every reason and fix is text that outlives the declaration. Acceptance: each such throw dies 76 with a sentence and its EXPLAIN row is gone; reason and fix are kept as spans, uncapped. Depends: habu-give-declaration-refusals-b0c396eb. Claim: unassigned.
