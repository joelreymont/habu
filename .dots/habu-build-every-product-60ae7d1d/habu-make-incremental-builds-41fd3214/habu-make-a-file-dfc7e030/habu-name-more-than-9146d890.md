---
title: Name more than 26 rows in facts text
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T23:32:10.944512+02:00"
---

Problem: src/core/render.f RATOM-CHAR (:215-216) names rows a-z only and answers ? past the 26th, setting RQM, and the parser's ROWMAP (src/core/checker.f:7647) holds 26 letters, so a sig-less word whose effect has more named rows renders no facts text and is not recorded: `: RW` calling CODE-RECLAIM:INVALIDATE! 14 times records nothing, so a caller such as `: RCALL ( -- ) [: drop ;]` 14 times then RW finds no effect for it. Every quotation literal with inferred rows names two rows, so 13 of them in one sig-less effect reach the cap: a word built of 14 \`[: dup ;]\` literals records nothing. Under C2c such a definition is refused as facts text it cannot say. No hb word hits it. Acceptance: a row name beyond Z (the parser's ROWMAP and the renderer agree on one spelling), and the RW/RCALL pair loads and RCALL checks. Depends: none. Claim: unassigned.
