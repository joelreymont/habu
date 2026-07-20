---
title: Checker diag for ! target mismatch lacks expected/actual
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T18:28:23.827560+02:00\""
---

Rejecting 0 <ptr-u8-word> ! prints only 'habu: in <word>: at !' with no expected/actual types (contrast drop-underflow diag 'expected: n actual:'). Reproducer: variable P : BASE ( -- ptr u8 ) P @ ; : CLR ( -- ) 0 BASE ! ; -> rc 70, one-line diag. Find the checker diagnostic path for store-target kind mismatch and emit expected/actual (expected: ptr a, actual: ptr u8) like other mismatch diags; add a diag-text regression.

Claim: agent=storediag workspace=.jj-ws/fable-storediag machine=spark (small engine lane: src/core/checker.f store-mismatch diagnostic + diag-text regression; CODELEN/census rows re-measured at merge if moved)
