---
title: Record an unsigned word that leaves its str local
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T09:59:08.525980+01:00"
---

Problem: ': Q4 {: s:str :} s USE-STR s ;' is W-EFFECT-NOT-RECORDED ('unmodeled type tag in inferred effect', src/core/render.f:2589), so a later call of Q4 is E-UNDEFINED; without the trailing s it runs. Reproducer /tmp/jerry-p1/q4/q4b.f. Acceptance: Q4 publishes ( str -- n str ) and q4b.f prints 5 hello. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-q4.
