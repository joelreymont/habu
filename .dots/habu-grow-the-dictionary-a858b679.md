---
title: Grow the dictionary past a fixed record count
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:46:03.735645+01:00"
---

Problem: src/habu/layout.f:151 fixes the dictionary at DICT-CAP 131072 records, and the hash index (HIDX-SLOTS, :452), the profiler's counter band (:161) and the image layout are sized from it, so a load that defines more words is rejected E-DICTIONARY-FULL ('define fewer words in this load'): a chain of 35726 one-field STRUCTUREs, each generating its words, fills it (S35726:MAKE). A limit Habu set itself, not one the machine imposes. Acceptance: a load defines words up to what memory allows, with no E-DICTIONARY-FULL from a record count; cold and warm builds and saved images agree; the dictionary-full test becomes a test of memory, or goes. Depends: none. Claim: unassigned.
