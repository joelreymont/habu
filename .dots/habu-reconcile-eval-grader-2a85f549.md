---
title: Reconcile eval grader dot
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T19:00:38.185839+02:00"
---

Problem: habu-eval-grader-isolate-1bdc9e86 is still active, but master appears to contain CHECK-CANDIDATE!, check-hook TRUST rows, and maki/eval.f CHECK-PASSES? calling CHECK-CANDIDATE!. The tracker may be stale and misleading. Fix: verify the dot acceptance exactly: repeated same-name candidates pass, core-collision names pass, USIGS/NORETS/SYM registry state is restored, maki eval/repair/fixture gates pass. Then close the stale dot if complete, or update it with the exact remaining failing invariant. Acceptance: dot state matches code reality; no active stale blocker remains; focused maki eval tests and full native gate evidence are recorded in the close/update reason.
