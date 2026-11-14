---
title: "A refusal's inferred effect and return stack show bindings made after the failure"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T12:47:37.174263+02:00"
---

Problem: a refusal's packet does not print every row as the refused token saw it. Expected, actual and declared are the token's rows, but inferred_effect and return_stack come from the live rows at the definition's end (src/core/render.f INFERRED-EFFECT :970). Case: ': X ( -- ) 1 IF THEN 2 ;' printed actual 'n n' at IF, where the stack holds one n. Inside a quotation literal INFERRED-EFFECT prints the definition's declared inputs against the literal's own rows ('n -- n [ a -- | -- a ]'), the effect of neither. Failures that clear OK without setting FAILSET (LIN-CHECK, CHECK-NO-BORROW's data base test and the other places in src/core/checker.f that clear OK) are pinned only by the end-of-token FAIL!, after their token ran ('( ltok n -- ) drop dup' prints 'ltok n -- ltok ltok' at dup); those decided at ; print the definition's end rows. Joel, 2026-11-10: "i want all issues identified to be dotted and fixed".

An unlanded attempt, uwzkpytk 3bbbe593 "Pin a refusal's rows at its first failure", was overturned by its review: a refusal raised while compiling (an undefined word, a stray then, a name imported with using) still printed the end rows because REFUSAL-TAKE's CAPTURE-CLEAR zeroes the pin; each kind pinned at a different point of applying its token, so the inferred row was before, during or after the token by kind; the walk after a pin was quadratic. The approach is redesigned before any fold.

Acceptance: every row a refusal prints is the row at the refused token, for every kind, including each place that clears OK without FAILSET (a census with a program for each, and a test per kind); inside a quotation literal, inferred_effect is the definition's or the literal's, stated in docs/forth.md beside the field and pinned by a test in test/compiler/input-underflow-refusals.f; the IF case above prints the rows at the token. Depends: none. Claim: unassigned.
