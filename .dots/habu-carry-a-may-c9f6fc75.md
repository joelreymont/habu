---
title: "Carry a may-flag across a stored xt's execute"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T15:54:31.876111+01:00"
---

Problem: an execute or catch of an xt held in a typed cell or passed as a quotation parameter drops the callee's may-flags. Renders: 'TYPED-VARIABLE XV [ -- ]' / ': EVC ( -- ) s" 1 drop" evaluate-closed ;' / "' EVC XV !" / ': Y ( -- ) XV @ execute ;' / 'Y': Y is not deferred as a direct call to EVC is. Throws: 'TYPED-VARIABLE XT [ n -- n ]' / ': T ( n -- n ) drop 7 throw ;' / "' T XT !" / ': R ( n -- n ) XT @ catch drop 1 + ;' is admitted by --load, tools/check.f and --verify-only, while ": R ( n -- n ) ['] T catch drop 1 + ;" is rejected E-STALE-READ; the parameter form ': R ( n [ n -- n ] -- n ) catch drop 1 + ;' is admitted too. Acceptance: each stored or passed form is judged as its direct form is. Depends: none. Claim: agent=Plan (design) workspace=none.
