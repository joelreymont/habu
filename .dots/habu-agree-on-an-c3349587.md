---
title: Agree on an evaluated is between the load and check.f
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T07:48:23.387762+01:00"
---

Problem: `: X ( -- ) ;  defer D ( -- )  : EV ( -- ) s\" is D\" evaluate-closed ; immediate  s\" EV\" 0 parse-imm  : P ( -- ) ['] X EV ;  P` loads with rc 0, but tools/check.f rejects it with E-MISMATCH 'P ends with [ -- ]': the pre-pass never learns that a program's own word is immediate (src/habu/verify-source.f `immediate` arm), so it walks EV as a call of ( -- ); without the parse-imm line the load gives E-UNMODELED-IMMEDIATE at EV and check.f the same E-MISMATCH. Acceptance: the load and check.f give one answer for both programs, the one the language's rules give; a test pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w204.
