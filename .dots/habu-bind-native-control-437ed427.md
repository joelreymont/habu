---
title: Bind native control words by identity
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T19:38:06.723729+03:00"
---

Problem: the native compiler classifies control words by spelling. `src/compiler/native/hir-word.f` declares `execute` (and the other control tokens: `catch`, `finally`, the C2 invoke, `'`, `evaluate`) as HIR controls through `MODEL-SYM ... BDECLARE-CONTROL`, and `elaborate.f` `DO-EXEC` then resolves `s" execute" NDICT:CALL-TARGET` by name. `docs/forth.md` (the `undefine NAME` rule) makes `undefine execute` followed by a top-level `: execute ...` a legal redefinition, and it is accepted (measured on the I7c fix lane, batch 13). The compiler then gives the program's own word the engine primitive's control lowering. `undefine` of a sealed trusted-only primitive is accepted the same way. Build drivers retire engine words on purpose (`src/habu/driver-io.f` `DRV-RETIRE-RELOADS`), so refusing `undefine` is not the fix.
Acceptance: first reproduce. `undefine execute : execute ( n -- n ) 1+ ;` followed by a tier-1 body calling `execute` on 41 should print 42 (the program's word). Record what each tier does today. Then each control token is classified as a control only when it resolves to the engine's own record (by identity, not by spelling). A program's replacement compiles at tier 0 and tier 1 as an ordinary call to that word, and the unreplaced words compile exactly as today. Cases: the reproduction, plus one replaced `catch`, in the suite that covers `undefine`. outer-interpret still agrees in both loops.
Files: `src/compiler/native/hir-word.f` (control declaration), `src/compiler/native/elaborate.f` (`DO-EXEC`, `DO-CATCH`, `DO-FINALLY`, `DO-C2-INVOKE`, `DO-TICK`, `DO-EVAL`), tier 0's keyword dispatch in `src/habu/habu2.f` if the reproduction shows it binds by spelling, and the suite.
Verify: the suite; spark chain gen2 == gen3 (compiler and habu2.f are baked); full gate.
Depends: none.
Ownership: krait.
Claim: unassigned.
