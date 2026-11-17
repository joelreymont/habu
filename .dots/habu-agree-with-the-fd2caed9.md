---
title: "Agree with the load on Maki's quotation escapes"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T09:05:31.138360+01:00"
---

Problem: the pre-pass publishes a definition whose body it leaves to the run with its declared signature through USIG-ADD-ROWS, whose KEEP-QUOTS counts every quotation input as kept (src/core/check-walk.f deferred publication, src/core/checker.f USIG-ADD-ROWS), so a caller handing it a quotation that reads a local gets E-LOCAL-ESCAPE, while the load checks the body and accepts. Maki's maki.f gets 11 such rejections (src/ops.f IN-CALL, IN-STATE; src/build.f OPS:EACH-ARTIFACT, VIEWS; src/step-source.f SESSION:WITH, NESTED). Reduction: a word rendered by evaluate-closed (GEN making MADE), `: R-EACH ( n [ n -- n ] -- n ) {: run :} MADE run execute ;`, `: R-USE ( n -- n ) {: k :} 0 [: k + ;] R-EACH ;`: load rc 0, check.f --verify-only E-LOCAL-ESCAPE at R-EACH. Acceptance: check.f --verify-only gives Maki no E-LOCAL-ESCAPE, and a program that fails today pins it in a test. Depends: none. Claim: unassigned.
