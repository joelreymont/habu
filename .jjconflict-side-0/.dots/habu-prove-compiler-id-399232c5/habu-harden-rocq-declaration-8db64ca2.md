---
title: Harden Rocq declaration inventory
status: open
priority: 3
issue-type: task
created-at: "2026-07-28T16:00:36.089853+02:00"
---

Full context: destruction review findings 8 and 9, LOW, brittleness not escapes. (a) PHASE-DECLARATIONS reads the four .v files with the shared Forth lexer (LINT-LEX): Rocq '(* ... *)' is not a Forth comment and a Rocq term 'apply ( f x ).' would make the lexer swallow to the next ')'. Today none of the four identity files contains a standalone '(' or a double quote, so the scan is accidentally exact - but Effects.v and Control.v already contain both hazards, so the coupling breaks the moment the inventory widens. Both failure directions red the gate rather than pass it, so this is robustness debt, not a hole. (b) DECL-HEAD? recognises only Theorem/Lemma/Example; Corollary, Proposition, Fact, Axiom, Parameter, Hypothesis, Variable, Section and Context are invisible to the inventory (an added Axiom is caught only via Print Assumptions on manifest-listed statements). Required result: a small Rocq-aware scanner - comment/string handling for Rocq syntax and a complete declaration-keyword set - either as a mode of LINT-LEX or a dedicated reader owned by COMPILER-ID-SRC, with hostile fixtures (declaration keyword inside a Rocq comment, inside a string, parenthesised application). Acceptance: the hostile fixtures pass/fail correctly; inventory counts unchanged on the current files; gate green.
