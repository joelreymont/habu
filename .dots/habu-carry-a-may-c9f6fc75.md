---
title: "Carry a may-flag across a stored xt's execute"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T15:54:31.876111+01:00"
---

Problem: an execute or catch of an xt held in a typed cell or passed as a quotation parameter drops the callee's may-flags. Renders: 'TYPED-VARIABLE XV [ -- ]' / ': EVC ( -- ) s" 1 drop" evaluate-closed ;' / "' EVC XV !" / ': Y ( -- ) XV @ execute ;' / 'Y': Y is not deferred as a direct call to EVC is. Throws: 'TYPED-VARIABLE XT [ n -- n ]' / ': T ( n -- n ) drop 7 throw ;' / "' T XT !" / ': R ( n -- n ) XT @ catch drop 1 + ;' is admitted by --load, tools/check.f and --verify-only, while ": R ( n -- n ) ['] T catch drop 1 + ;" is rejected E-STALE-READ; the parameter form ': R ( n [ n -- n ] -- n ) catch drop 1 + ;' is admitted too. The same cause admits more: a deferred word's catch (': R ( n -- n ) [: DT ;] catch drop 1 + ;' with DT set to a thrower); a passed quotation run inside a catch (': A ( n [ n -- n ] -- n ) execute ;' / ': R ( n -- n ) [: [: T2 ;] A ;] catch drop 1 + ;', where '5 R .' prints 1006); a returned quotation (': MK ( -- [ n -- n ] ) [: T ;] ;' / ': R ( n -- n ) MK catch drop 1 + ;'); a join of a thrower and a non-thrower (': JA ( n bool -- n ) if [: 1 + ;] else ['] T2 then catch drop 1 + ;'). And ': MK [: T ;] ;' makes --load exit 76. Acceptance: each stored, passed, returned or joined form is judged as its direct form is; the unsigned MK is rejected or admitted with rc 0 or 70, never 76. Depends: none. Claim: agent=carl (design) workspace=none.
