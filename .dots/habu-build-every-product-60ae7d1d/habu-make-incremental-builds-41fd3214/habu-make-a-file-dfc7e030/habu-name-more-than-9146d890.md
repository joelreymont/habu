---
title: Name more than 26 rows in facts text
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T23:32:10.944512+02:00"
---

Problem: src/core/render.f RROW-LETTER names rows A-Z only and answers ? past the 26th, setting RQM, so a sig-less word whose effect has more named rows renders no facts text and is not recorded: `: RW` calling CODE-RECLAIM:INVALIDATE! 14 times records nothing, so a caller such as `: RCALL ( -- ) [: drop ;]` 14 times then RW finds no effect for it. Every quotation literal with inferred rows names two rows (C2b1's kind rule), so 13 of them in one sig-less effect reach the cap: a word built of 14 \`[: dup ;]\` literals records on master b5f2b81b and not after C2b1 (re-review, /tmp/rev-c2b1r.jCX6/mq.f). Once C2c lands such a definition is refused as facts text it cannot say. No hb word hits it (C2b1 probe: 0 unrenderable live rows). Acceptance: a row name beyond Z (the parser's ROWMAP and the renderer's RROW-LETTER agree on one spelling), and the RW/RCALL pair loads and RCALL checks. Files: src/core/render.f, src/core/checker.f. Verify: the RW/RCALL file loads; the facts probe stays same on every live row. Depends: C2c (habu-c2c-store-a-1bfc2ab6). Ownership: those files. Claim: unassigned.
