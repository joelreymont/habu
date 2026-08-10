---
title: Compile a quotation body that never returns
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T16:34:19.393832+02:00"
---

The last [: refusal in the tree: lib/test/suite.f DEFAULTS holds [: RUN-MISSING ;] whose body never returns (RUN-MISSING is certified dead). A body-as-function with no return needs: what the emitter writes for it (the no-return exit-block rule exists for TRAPS - a dead-CALL body ends in its call plus the unreachable trap, same as any dead path - verify the all-dead-function publication question habu-mint-a-routine-bfebc8bd already owns is the same question and DEPEND on it if so), and what a caller may do with its address. Reproduced and pinned in the S1 suite (quot core lane 2026-08-11). Files: src/compiler/native/{elaborate,emit}.f. Depends: habu-mint-a-routine-bfebc8bd (verify before claiming).

OVERLAP ANSWERED 2026-08-11 (noret lane): NORET-FRAMED does NOT serve the
non-returning quotation BODY - a contract describes function zero only
(DECL-CK/FUN-PLACES scope it there; VNORET-CK deliberately matches), and a
body's frame/link rules come from the contract applied per function. A
non-returning quotation body needs a PER-FUNCTION control statement no
contract field expresses. This dot's work is that statement, not the form.
