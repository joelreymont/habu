---
title: "Name an unsafe word's refusal in plain-text diagnostics"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T18:08:04.665727+02:00"
---

Problem: a checked body naming a word UNSAFE-TOK? refuses (set-check, evaluate) prints only 'habu: in xd: at 'set-check'' with no code or reason, on master and every candidate (lane dfrsh review, /tmp/jerry-dfrshrv-tmp/q1.f, q3.f, q5.f). render.f DIAG-PROSE has no RF-UNSAFE arm, while the JSON form already gives E-UNSAFE with its suggestion and docs/forth-card.md section 3 promises E-UNSAFE. Acceptance: the plain-text line names E-UNSAFE and the reason, as an RF-CAP refusal's does; a row in the suite that checks --load diagnostics, failing on master. Files: src/core/render.f, the row's file. Verify: engine build; rows program-diagnostics, load-reject-diag. Ownership: render.f DIAG-PROSE. Claim: unassigned.
