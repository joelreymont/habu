---
title: "C2c-d: find why a value record's field cells do not read back"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T01:33:35.659961+02:00"
closed-at: "2026-11-07T04:02:42.489278+02:00"
close-reason: "implemented, reviewed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log)"
---

Problem: the C2c candidate (/tmp/jc2.dP/hb, full suite /tmp/jt.Ry/suite.log) refuses `: T->POINT ( n n -- point ) ;` after `VALUE-RECORD point x n y n END-VALUE-RECORD` with `hook: facts text differs: t->point: ( n n -- @:field<point,x,n> @:field<point,y,n> )` (test/engine-suite.f:1054-1058; also test/c2-retained-contract-wide.f, suite c2-view-record-source F20); the parser refuses `@:field<...>` in a signature (/tmp/jerry-big/c2c-repro/r2.f). Acceptance: a reported cause with file:line and the rule the fix follows; the fix is a second brief, a commit under nkwlmvos. Files: read-only. Verify: /tmp/jerry-big/c2c-repro/r.f on the candidate. Depends: none. Brief: /tmp/jerry-big/brief-rca-rec.md. Ownership: jerry. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-rca-rec.

Finding (worker, 2026-11-07, /tmp/rca-rec.6A): a value record's cells are `field` family applications whose first two arguments are plain atoms (checker.f:7629-7639 VREC-FIELD-WRAP); render.f:237-242 ATOM-REND writes them bare and checker.f:6543-6552 TOK-TYPE reads a bare token as a type, so the facts parse of `( n n -- @:field<point,x,n> ... )` is refused. The record's name cannot replace the run: `VALUE-RECORD box value a` reads `( box -- box )` with a fresh variable per `box`, the certified record shares one. Lead's rule: facts text writes every plain atom as `'NAME`, read in facts mode only. Fix (lane, brief /tmp/jerry-big/brief-fix-rec.md, workspace .jj-ws.noindex/jerry-rca-rec on nkwlmvos, moved below it at landing).
