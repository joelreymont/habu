---
title: Agree on an evaluated is between the load and check.f
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T07:48:23.387762+01:00"
---

Problem: `: X ( -- ) ;  defer D ( -- )  : EV ( -- ) s\" is D\" evaluate-closed ; immediate  s\" EV\" 0 parse-imm  : P ( -- ) ['] X EV ;  P` loads with rc 0, but tools/check.f rejects it with E-MISMATCH at EV: docs/forth.md "Rules learned by refusal" has the check take a body as if its parse-imm immediate were not there, while the load's check walks the `is D` that EV, run by src/habu/interpreter.f DEF-RUN, evaluated into P. A parse-imm immediate that changes the body it runs in makes the declaration's model false, so --verify-only certifies bodies the load compiles differently. Acceptance: the load rejects, at the immediate and with E-UNMODELED-IMMEDIATE, a parse-imm immediate that changed the body it ran in, by tokens it evaluated or code it compiled; docs/forth.md and docs/forth-card.md say a parse-imm immediate compiles nothing; a test pins the rejection, and tests whose subject is such an immediate expect it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w204.
