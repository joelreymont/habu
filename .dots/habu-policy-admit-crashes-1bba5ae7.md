---
title: policy-admit crashes in a stripped program
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:43:15.524417+02:00"
---

Problem: a program built by the default build (tools/hb-build-core.f APP-BODY, stripped) that calls policy-admit crashes inside WLFIND:LENTRY's linear scan (x5=x26=1, x27=0xffff), on the installed Habu ee4b1361 and on the row-walk candidate alike; found by the row-walk review with a scratch program (7.4 s build), reproducer not kept. A stripped program also never runs RUNTIME:INSTALL, so POLICY-ABI:KEYWORD-CELL is 0 there once the row walk lands. No consumer (zen, tender, maki) names policy-admit. Acceptance: a stripped program that admits a package and seals runs as the engine does, or the build refuses a stripped program that names policy-admit with a message. Files: src/habu/habu1.f (BPOLICYADMIT), tools/hb-build-core.f (APP-BODY, END-WRITE), src/lib/vocabulary.f. Verify: a built-program case in src/lib/vocabulary-test.f. Depends: the row walk (habu-delete-the-jit-d9fdaf67) landing first. Ownership: those files. Claim: unassigned.
