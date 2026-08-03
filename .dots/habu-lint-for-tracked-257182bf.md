---
title: Lint for tracked build outputs
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:14:17.068812+02:00"
---

Full context: 32 regenerable build outputs under formal/ were tracked for weeks despite matching ignore rules that were already present — ignore rules NEVER apply to already-tracked files, so nothing caught it and three lanes had to hand-strip the churn from commits. Add a checked Habu lint that walks the tracked file list and fails if any tracked path matches known build-output patterns (formal/**/*.vo, .vok, .vos, .glob, .aux, .timing, .lia.cache, formal/Makefile.rocq*, plus *.o, __pycache__ and similar), and enroll it in the commit gate. Without it the ignore file is advisory and this recurs with the next generated artifact. Acceptance: the lint reds when a build output is force-tracked, is green on the clean tree, and is scheduled in test/gate-stdlib-cases.f and test/gate-stdlib-inline-lib.f.
