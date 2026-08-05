---
title: Give the immediate contract table a consumer again
status: open
priority: 1
issue-type: task
created-at: "2026-07-31T19:07:38.855056+02:00"
---

Full context: src/compiler/native/elaborate.f used to ask src/compiler/native/immediate.f (package NIMM) exactly one question - is the definition-frame word ':' or ';' a declared front-end intrinsic - and that question died with the frame: a produced tape has no frame rows, so NELAB no longer requires immediate.f at all and NIMM has no production consumer left, only its own suite (test/compiler/native-immediate.f). The table is not wrong, it is unasked. The real question it should answer is about BODY words: NELAB:STEP today hands every body token straight to HIR-WORD:ADMIT-TOKEN, so a token that IS a declared immediate - an 'if', a 'postpone', a named unmodeled boundary - is refused as E-HIR-UNMODELED ('this dialect does not model that word') instead of E-NIMM-UNMODELED ('that immediate has no elaboration contract, and here is the capability that has to land'), which loses the capability name design section 7.1 asks the three classes to carry. Acceptance: a body token is classified against the immediate table before the word model, an immediate with no contract is refused with NIMM's own name and its recorded reason symbol, a compile-time immediate is refused by name until habu-seal-the-compile-5f56e5e9 lands, and test/compiler/native-elaborate.f carries a fixture for each of those three shapes. Discovered while landing habu-reconcile-the-produced-26737779.
