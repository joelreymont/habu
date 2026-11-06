---
title: "C2c-g: spell a type variable past the letters by its ordinal"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T02:28:13.543057+02:00"
closed-at: "2026-11-07T04:02:42.548181+02:00"
close-reason: "implemented, reviewed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log)"
---

Problem: facts text names type variables and binders with render.f RLETTER's 23 letters; the 24th renders ? and sets RQM, so check-hook.f refuses a declared word ('hook: facts text cannot say') and REC-SIG leaves a sig-less one unrecorded. test/c2-retained-contract-wide.f:4 C2V-DROP takes 33 box (VALUE-RECORD box value a), each its own variable, so c2-view-record-source fails on C2c (green on master). Acceptance: past the letters facts text writes the decimal ordinal (23, 24, ...) up to TYPE-VAR-MAX 256 names, read only in facts mode (checker.f SIG-VAR-SLOT, SLOT-VAR; TOK-TYPE, binder and parent names, TAG-VAR); render.f VAR-NAME, SEEN-IDX, VAR-FACT-TAG replace RLETTER/LET-OF; RBIND and BIND-DOMAINS sized BINDER-DEPTH-MAX 32; c2-view-record-source passes; engine-suite RSV28/RSB24 record and are callable, RSQ276 (12 x T-Q23) refused 'more than 256', RSO23 ( 23 -- 23 ) still refused in source. Files: src/core/checker.f, src/core/render.f, test/engine-suite.f. Verify: brief /tmp/jerry-big/brief-c2c-g.md (candidate build within 5 minutes, facts census against the uztvlzwq baseline, named tests). Depends: C2c-f (habu-c2c-f-find-19adf6ec). Ownership: those files. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c2cg.
