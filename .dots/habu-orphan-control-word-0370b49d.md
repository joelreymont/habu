---
title: Orphan control word in definition crashes SIGBUS
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T19:37:15.034802+02:00\""
---

Found by the evalcatch lane 2026-07-15 while verifying habu-def-compile-failure-7182eeb2: a definition containing an orphan control word - minimal repro ': XI ( -- ) THEN ;' - crashes SIGBUS rc 134 with a register dump at PLAIN TOP-LEVEL stdin (and under catch+evaluate), unlike every other def-compile failure class (undefined, min-in, output mismatch, non-certified, wrong depth) which exits orderly rc 70 / throws catchable 70 under eval. This is a UNIVERSAL compile crash, not the eval-boundary bug (it fails 7182eeb2's own discriminator), so it needs its own RCA. Fix: debugger-evidence-first (docs/debugging.md stepper/lldb on the die path - what does THEN's compile action dereference when no IF opened the control frame?), then make orphan control words a clean rejected compile (catchable code, rc-70 contract at top level) for THEN/ELSE/REPEAT/UNTIL/AGAIN/LOOP/+LOOP and any other closer, with red-first engine-gate regressions per word (rc 134 before, rc 70 orderly after; catch sees the code under eval). Engine change: fixpoint x2 byte-identical + bootstrap mirror if the touched region is mirrored + full run.f. Files: src/habu/habu2.f (or the control-flow compile words' owner), engine gate regressions, bootstrap/cg/forth.fs if mirrored. Ownership: engine compile-failure paths.

Claim: agent=orphanctl workspace=.jj-ws/fable-orphanctl
