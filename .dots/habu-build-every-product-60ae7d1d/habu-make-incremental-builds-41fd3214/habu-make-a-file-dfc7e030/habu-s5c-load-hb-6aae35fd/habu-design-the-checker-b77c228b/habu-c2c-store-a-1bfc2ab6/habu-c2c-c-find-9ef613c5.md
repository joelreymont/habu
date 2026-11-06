---
title: "C2c-c: find why facts mode refuses three cast texts"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T00:41:14.526672+02:00"
closed-at: "2026-11-07T04:02:42.460967+02:00"
close-reason: "implemented, reviewed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log)"
---

Problem: on the C2c-a candidate (/tmp/jc2.GN/hb) the facts probe /tmp/jc2.Mo/fid-new.f leaves three records parse-refused (/tmp/jc2.GN/new.raw:24529, 24530, 24550): ( ptr u8 n -- read-view<a,b,u8> ), ( ptr u8 n -- mut-view<a,b,c,u8> ), ( ptr u8 n -- mut-view<a,b,c,init<b,c2-mem:owner-state>> ), the texts of CAST: READ-PACK and MUT-PACK (src/lib/c2-memory.f:41-42) and OWNER-VIEW-PACK (src/lib/c2-owner.f:13). Each has a type variable only among its outputs (hypothesis). C2c's refusal would fire on them if CAST: rows reach CHECK-FACTS. Acceptance: a reported cause with file:line, the rule the fix follows, and whether C2c's check reaches CAST: rows; the fix is a second brief. Files: read-only. Verify: the worker's reproducer under /tmp/jrv.*. Depends: C2c-a (habu-c2c-a-facts-23c32585). Brief: /tmp/jerry-big/brief-rca-view.md. Ownership: jerry. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-rca-view.

Finding (worker, 2026-11-07, /tmp/jrv.ev): PSIG (src/core/checker.f:7075 at mpvwpvrl) applies the source rule that every scope or region output variable is supplied by an input (SCOPE-SUPPLIED?, SGBAD-UNSUPPLIED kind 6) in facts mode too; the three pack casts' outputs name the view's scope variables, so the facts build stops. In source the rule is an admission rule: CHECKER-IDENTITY:PARSE (:27240) holds it back so CAST admits a view pack in C2-MEM's private section only, and a colon definition with such a signature is refused rc 70. C2c's check does not reach CAST: rows (CHECK-FACTS runs only from the colon hook; /tmp/jrv.ev/hook-reach.f), but every later facts reader would refuse them. The probe's `no-record` means the three symbols are global with an empty name. Fix (lane, brief /tmp/jerry-big/brief-fix-view.md, workspace .jj-ws.noindex/jerry-rca-view on mpvwpvrl): PSIG skips the supplied check under SIG-FACTS-MODE, as PUSH-LOGICAL (:6827) gates its source rule.
