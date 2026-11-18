---
title: Build a program that undefines one of hb's words
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T23:40:35.189177+01:00"
---

Problem: a program file that undefines one of hb's own words, written ('undefine dup') or evaluated, ends its build with E-MEMORY-FAULT in DICT-CELL@, rc 110: src/habu/linker.f ID-REF+ (:776) has no record reference for a record below FIRST-REC, so FACT+ (:2466) writes the retirement as BUILT row 0's (file 1, sum 0, count 0), and LINK-RETIRED (:4964) replays it as record -1 (FACT-REC :4955, CHANGER+ :2331). Acceptance: such a program builds cold and warm and runs; a later file that spells the retired word is rejected E-UNDEFINED in a warm build as in a cold one; a test in test/file-objects.f holds both. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-uh.
