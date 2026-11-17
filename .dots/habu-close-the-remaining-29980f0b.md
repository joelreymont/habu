---
title: Close the remaining checker registrars to programs
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T22:43:13.070718+01:00"
---

Problem: carl's store stack (dot c0627710) closes the rest of what a program could reach to rewrite recorded checker facts, but three words stay visible to a program: the public table CHECKER-REG:DECLARATIONS (src/core/checker.f), whose rows hold TRUST-DECL-REJECTED?, TRUST-DECL? and DECLARED. With a private 'CAST: >ROW? ( n -- [ ^u8 n ^u8 n -- bool ] )', ': TWO ( -- n ) 2 ;', then 's" TWO" s" -- n n" CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:DECL-ROW-OFF + @ >ROW? execute drop', then ': USE ( -- n ) TWO + ;', the program loads and USE underflows at run time, rc 67. TFAM:SUMV-CTOR-SYM! (src/core/type-family.f) registers any symbol as a generated constructor, which lifts linear conservation for that word (checker.f, the CTOR-ROW? use). BODY-ITEMS:EFFECT-CELL (src/habu/layout.f) is public; it is the cell holding the effect read for a body's signature. carl's probes, /tmp/carl-jq/p*.f, covered five of the xt! registrars in checker.f, check-walk.f and check-hook.f, not the others. ROOT-MOVER (src/core/include.f) is global and tags a program's own last word; it must follow INTRINSIC's route, both the interpreter.f build routing and the closing. Acceptance: none of these words, and no unprobed registrar, is visible to a program; privacy only, a private section, no caller check. Each probe is E-UNDEFINED at the name it uses, rc 70. Depends: c0627710. Claim: unassigned.
