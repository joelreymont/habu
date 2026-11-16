---
title: "Keep an evaluated operand's construct in its record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T07:48:23.368663+01:00"
---

Problem: src/habu/interpreter.f DEF-PREFIX-BIND sends every operand that fails to resolve during an evaluate inside a definition to UNDEFINED, losing which construct read it: an evaluated `is if` gets "cannot find word `if`" with help 'define `if` before this point', which no `is` target can satisfy, while the same `is if` written in a body gets '`if` names no word' with defer help. Reproducer: `: X ( -- ) ;  defer D ( -- )  : EV ( -- ) s\" is if\" evaluate-closed ; immediate  `parses: EV 0`  : P ( -- ) ['] X EV ;` (/tmp/jerry-c33/isif.f). Acceptance: an evaluated operand that fails to resolve gets the record its construct gives when written in a body; a test pins the `is` case. Depends: none. Claim: unassigned.
