---
title: Fold the conditional-select comparison too
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T09:00:00.000000+02:00"
---

Measured residue of the cmp-immediate fold (da4cc639, merged 54461eda; measurement on that leaf's history): every comparison the fold leaves on the register path is an a64.cmpsel - the if-conversion turns a small two-armed body into a conditional select, a third machine form the cmp dot did not name. SYM-FOLD-C keeps 2, LADDER keeps 2 (~16 more bytes on scored rows), plus several in TV-NEXT?. An a64.cmpseli is the same machinery again: fold operand 1 into the immediate field, four operands -> three; same encoder field (ENC-CMPI's 12-bit unsigned), same single-use test, same combine-pass home, same mutation set. Files: src/compiler/native/{a64ir,combine,emit,spill}.f. Depends: none (the cmp landing is the worked example).
