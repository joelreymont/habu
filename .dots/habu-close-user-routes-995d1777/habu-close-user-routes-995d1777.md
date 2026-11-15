---
title: "Close user routes into the checker's effect store"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T18:19:42.322297+01:00"
---

Problem: user source still writes the checker's effect store through words that stay global after the row words went private. (1) E-ADD-EFFECT (src/core/checker.f), CHECKER-REC-SYM, PE-SYM-OF, FRESH, MK-ROW, MK-PUSH, PE-N, PE-PTR: `: FOOB ( -- n ) 1 ;  s" FOOB" PE-SYM-OF CHECKER-REC-SYM !  FRESH MK-ROW dup PE-N PE-PTR swap MK-PUSH 0 0 false false E-ADD-EFFECT  : T ( -- n ) FOOB @ ;  T . cr` admits T, which faults at run time (rc 110); without the second line T is rejected. (2) CHECK:PRIM-OPEN, PRIM-ATOM, PRIM-CLOSE, PRIM-NAME, PRIMS-END (src/core/check-walk.f), called only by src/habu/linker.f and src/arch/arm64/link-kernel.f: `: FOOB ( -- n ) 1 ;  s" FOOB" CHECK:PRIM-OPEN 8 CHECK:PRIM-ATOM 2 CHECK:PRIM-ATOM s" " true false CHECK:PRIM-CLOSE  : T2 ( -- n ) FOOB ;` adds a trusted-only row that rejects T2, and `s" T:R" CHECK:PRIM-OPEN s" " false false CHECK:PRIM-CLOSE : P6 ( -- ) ; CHECK:PRIMS-END` kills hb (rc 76). (3) TRUST (src/core/checker.f:19698) believes any row it is given: `: FOOB ( -- n ) 1 ; s" FOOB" s" -- n n" TRUST : T ( -- n ) FOOB + ; T . cr` admits T, which dies with rc 70. E-BUILD-EFFECT writes the store as E-ADD-EFFECT does; E-ADD-DELETED, CHECK:UNDEFINE and CHECKER-UNDEFINE make a defined word undefined to the checker. Acceptance: both programs are rejected as undefined under --load and under tools/check.f, as a test; no word user source can call writes the effect store or states a row. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-store.
