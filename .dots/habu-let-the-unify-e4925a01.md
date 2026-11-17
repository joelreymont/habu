---
title: Let the unify worklist grow past 1024 pairs
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T14:47:30.150201+01:00"
---

Problem: src/core/checker.f:2270 MAXUWL sizes the unify worklist at 1024 pairs with no grow path; U-PUSH (:2319) and the forall input worklist (:4460) die 'worklist full' as an internal error, while a stack effect may hold EFFECT-DEPTH-MAX (4096) entries, so a program with a wide effect may stop the checker. Acceptance: a reproducer shows whether a program reaches the cap; if it does, the worklist grows with its input and the reproducer checks. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-uwl.
