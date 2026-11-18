---
title: Use align and the scratch definers in the library
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T17:39:11.707144+01:00"
closed-at: "2026-11-18T17:41:51.571402+01:00"
---

Problem: library files align with the idiom 'here data-base - negate 7 and allot' (and task.f's TASK-ALIGN8) and declare scratch cells with 'variable X  X scratch-cell-mark', so they need data-base and scratch-cell-mark, which the parent dot takes from programs. Acceptance: no file under src/lib or test/ uses that idiom or TASK-ALIGN8, and the scratch sites the brief names use SCRATCH-VARIABLE or SCRATCH-ALLOT. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-d5.
