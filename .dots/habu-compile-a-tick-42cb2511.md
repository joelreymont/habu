---
title: Compile execute of a row-effect xt at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T17:26:47.441237+02:00"
---

Problem: native tier 1 refuses executing the tick of a word whose effect has stack rows. Repro (load with bin/hb --load): '1 set-tier  package VIAQ  : ROWS ( R n [ R n -- S ] -- S ) execute ;  : ROWS-VIA ( -- ) 5 [: dup 3 + ;] ['] ROWS execute swap 10 * + throw ;  ;package' prints 'ncomp: cannot compile ROWS-VIA' and 'hb: uncaught throw code -8304' (E-NELAB-UNDER, src/compiler/native/elaborate.f:190-263 throw sites; DO-EXEC :3466). The same shape with ': ONE ( n [ n -- n n ] -- n n ) execute ;' compiles. A typed cell takes the same refusal: '1 set-tier  package VIAC  : AFTER ( R n [ R n -- S ] -- S ) execute [: ;] execute ;  1 TYPED-BUFFER AFTER-CELL [ R n [ R n -- S ] -- S ]  ' AFTER 0 AFTER-CELL !  : VIA-CELL ( -- ) 5 [: dup 3 + ;] 0 AFTER-CELL @ execute swap 10 * + throw ;  ;package' prints 'ncomp: cannot compile VIA-CELL' and -8304; with ONE's row-free type the same route compiles and throws 58. Measured 2026-11-09 on the installed engine (master 57ea97fb sources). The checker accepts the definition; the elaborator refuses it. test/wasm/dynamic.f's AFTER-VIA-EXECUTE casts the tick (>GROW) because of this. Acceptance: ROWS-VIA and VIA-CELL compile natively and throw 58; the >GROW cast in test/wasm/dynamic.f goes. Files: src/compiler/native/elaborate.f, test/wasm/dynamic.f. Verify: the repro, then bin/hb --load test/wasm/dynamic.f rc 0 without the cast. Depends: none. Ownership: src/compiler/native/elaborate.f. Claim: unassigned.
