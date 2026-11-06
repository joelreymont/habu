---
title: "C2c-h: fold the ordinal spelling's review fixes"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T03:21:50.271767+02:00"
closed-at: "2026-11-07T04:02:42.563858+02:00"
close-reason: "implemented, reviewed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log)"
---

Problem: the C2c stack review (/tmp/jerry-big/review-c2c-triage.md) found in szouqtzz a non-conversion name SLOT-VAR, literal counts in REC-REFUSE-WHY's reasons (256, 26), T-FORALL's binder-depth refusal reported as 'unmodeled type tag', and card section 2 layout breaks. Acceptance: SLOT>VAR; REC-REFUSE-WHY an emitter writing TYPE-VAR-MAX, RROW-CAP and BINDER-DEPTH-MAX through JNUM with unchanged bytes; RBIND-FULL latch naming 'more than 32 nested binders'; layout per brief. Brief: /tmp/jerry-big/brief-fold-szou.md. Files: src/core/checker.f, src/core/render.f, test/engine-suite.f. Verify: candidate build within 5 minutes; fid-new probe; bin/hb --load test/run.f -- engine type-decl c2-view-record-source checker-soundness diag-position engine-stack-wide c2-diagnostics program-diagnostics. Depends: none. Ownership: those words. Claim: agent=worker workspace=.jj-ws.noindex/jerry-fold.
