---
title: Give the immediate contract table a consumer again
status: closed
priority: 1
issue-type: task
created-at: "2026-07-31T19:07:38.855056+02:00"
closed-at: "2026-08-20T21:01:45.819061+02:00"
close-reason: "Superseded by the 2026-08-20 hard cut: NIMM deleted (habu-delete-the-nimm-329100c9) instead of rewired; the compile-time class re-derives from history when it has a named first consumer."
---

Full context: src/compiler/native/elaborate.f used to ask src/compiler/native/immediate.f (package NIMM) exactly one question - is the definition-frame word ':' or ';' a declared front-end intrinsic - and that question died with the frame: a produced tape has no frame rows, so NELAB no longer requires immediate.f at all and NIMM has no production consumer left, only its own suite (test/compiler/native-immediate.f). The table is not wrong, it is unasked. The real question it should answer is about BODY words: NELAB:STEP today hands every body token straight to HIR-WORD:ADMIT-TOKEN, so a token that IS a declared immediate - an 'if', a 'postpone', a named unmodeled boundary - is refused as E-HIR-UNMODELED ('this dialect does not model that word') instead of E-NIMM-UNMODELED ('that immediate has no elaboration contract, and here is the capability that has to land'), which loses the capability name design section 7.1 asks the three classes to carry. Acceptance: a body token is classified against the immediate table before the word model, an immediate with no contract is refused with NIMM's own name and its recorded reason symbol, a compile-time immediate is refused by name until habu-seal-the-compile-5f56e5e9 lands, and test/compiler/native-elaborate.f carries a fixture for each of those three shapes. Discovered while landing habu-reconcile-the-produced-26737779.

CLOSED BY THE USER'S HARD-CUT RULING (2026-08-20). The rewire never happened
and this leaf was the only thing keeping the table alive, which is the Simplify
gate's own definition of parked machinery: no named first consumer, no
residency. So the table was deleted instead of rewired, by
habu-delete-the-nimm-329100c9. The design question this leaf framed is not
answered and does not need to be until an immediate word actually needs it:
today a body token the dialect cannot compile is refused as E-HIR-UNMODELED by
src/compiler/native/hir-word.f, which names the word and, for a declared
boundary, the capability it waits on. When a compile-time immediate has a real
first consumer, the classifier re-derives from history: the whole deleted set -
src/compiler/native/immediate.f, test/compiler/native-immediate.f, the
-8220..-8239 error block, the suite row and the fork row - is one `jj restore`
away at the deletion commit, and the sized list is in this repository at
.dots/habu-delete-the-nimm-329100c9.md.
