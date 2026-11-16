---
title: "Keep an evaluated operand's construct in its record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T07:48:23.368663+01:00"
---

Problem: src/habu/interpreter.f DEF-PREFIX-BIND (:2048-2058) sends every operand that fails to resolve during an evaluate inside a definition to UNDEFINED, losing which construct read it: an evaluated `is if` gets the keyword record's help 'name a word defined with `:` … `;` that uses `if`', which no `is` target can satisfy, while the same `is if` written in a body gets '`if` names no word' with defer help. Reproducer: `: X ( -- ) ;  defer D ( -- )  TRUSTED: EV ( -- ) s\" is if\" evaluate ; immediate  s\" EV\" 0 parse-imm  : P ( -- ) ['] X EV ;`. Acceptance: an evaluated operand that fails to resolve gets the record its construct gives when written in a body; a test pins the `is` case. Depends: none. Claim: unassigned.
