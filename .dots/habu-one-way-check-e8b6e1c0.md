---
title: One way check decides a reported refusal
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:27:50.804937+02:00"
---

Problem: after chk-2 (carl-chkscan sltotuvv), plain mode decides that a refusal already rendered its packet two ways: by the event (CHECKER DEF-STOPPED, set where a definition or declaration refusal renders) and by REPORTED-ERROR?'s code list (src/core/checker.f 26918) behind CHECK-ALL-ERRORS:THREW? (tools/check-all-errors-core.f 482-484) for generates:, parses:, names: rows, casts and others. A list drifts from the events it stands for. Acceptance: every refusal that renders its packet sets the event; THREW? asks only the event; REPORTED-ERROR? is gone; the event is set by one checker word ("count the reject in a multi-error load, else record the stop and refuse") that DEF-REFUSED, TYPE-DECL:TDECL-RUN and DECL-REJECT:GUARD-CODE call, so no other package writes DEF-STOPPED (review of chk-1 + chk-2, finding 6: the three now repeat the rule in three files). Files: src/core/checker.f, tools/check-all-errors-core.f, tools/check-verify-child-lib.f. Verify: the chk probe table (~/.cache/tmp/carl-chkscan/c1/run.sh, sum.sh) unchanged; test/diag-position-test.f rc 0 under load and plain check. Depends: dot 8ec38573 lands. Ownership: REPORTED-ERROR?, THREW?. Claim: unassigned.
