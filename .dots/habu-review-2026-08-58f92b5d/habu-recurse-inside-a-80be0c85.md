---
title: RECURSE inside a quotation calls the quotation
status: closed
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.887038+02:00"
closed-at: "2026-08-23T14:20:11.688766+02:00"
close-reason: implemented, reviewed, merged, gates green: a quotation's RECURSE is lowered to function zero - SELF-SHAPE takes the contract's arity, PUT-CALL branches to SELF-FUN FUN-START, VDNET-CK checks VD-SELF (4ccb6fae, landed via merge 6ecb02fe; engine refreshed to the byte fixpoint at the merge); the checker was correct all along (RCA owner = codegen model); QWALK-CASE red 100/100/100/100 -> green 100/200/300/400, QWIDE-CASE red E-A64SEL-CALL -> green 0 30; six native suites, maki, judge --check (46 rows unchanged), both diff lints green.
---

Problem: src/compiler/native/elaborate.f:2938-2947 DO-SELF-CALL stages hir.call with the definition's arity and QBUILD (3438-3461) walks quotation bodies through the same STEP; select.f:848-861 SELF-SHAPE reads the CURRENT function's ARGS/OUTS (the quotation's for ord<>0); emit.f:1229-1233 PUT-CALL branches to block 0 of the function being written - in a quotation that is the quotation's own entry. The engine's J-RECURSE (habu2.f:2542) calls PEND (the open definition); the checker's CF-RECURSE (checker.f:10050) has no QDEPTH guard so the body certifies. When arities match (the tree-walk shape '( node -- ) [: ... RECURSE ;]') the chain publishes a routine whose quotation recurses into itself; no test covers RECURSE inside a quotation. Acceptance: Checker-Miss RCA; STEP/SK-STEP refuse self-call when QCUR <> QOWNER-DEF via QUOT-REFUSE until a wordcall to function 0's entry is lowered (or lower it); a native-chain test with the shape shows the published routine equal to the interpreted word. Files: src/compiler/native/elaborate.f, select.f, emit.f, test/compiler/native-chain.f. Verify: the test red on the old chain, green after. Depends: none. Ownership: native chain. Claim: closed (landed on master via 6ecb02fe).
