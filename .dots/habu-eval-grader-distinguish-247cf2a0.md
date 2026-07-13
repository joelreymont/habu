---
title: "Eval grader: distinguish infra failure from candidate rejection + judge-image vocab tripwire"
status: active
priority: 2
issue-type: task
created-at: "2026-07-13T18:42:49.475518+02:00"
---

Problem (liveeval review advisory A1): (1) EE-EMIT (maki/eval-emit.f:85-90) discards the child's rc and stderr and returns only the captured PTX byte count; a certifying candidate always emits scaffold PTX, so zero output means the emit infrastructure failed (missing/broken child bin/hb, crash) — but GRADE-* maps zero output to grade 1 (TYPED-WRONG), misattributing a broken grader as a wrong candidate. Fix: EE-EMIT throws a named maki-eval error (echoing EE-ERR stderr) on zero output instead of returning 0. (2) The judge-image missing-vocab failure (maki/eval-matrix-main.f) is fixed-for-now (c1744541 requires cg-matmul.f/cg-attention.f) but any FUTURE task whose PTX vocabulary is outside the judge image silently grades 0 again (fail-open). Fix: a task->vocab manifest or a suite tripwire that replays a phase-word transcript through eval-matrix-main.f's own load path so a dropped require can't silently zero a task. Acceptance: a fixture where a deliberately-broken grader child throws (not grades 1); a fixture proving a missing judge-image vocab is a loud failure, not silent 0. Files: maki/eval-emit.f, maki/eval-matrix-main.f, lib/errors.f (new E-EVAL code), tests. Verify: maki/test.f. Depends: none. Ownership: eval grader infra. Claim: agent=eval_grader workspace=.jj-ws/eval-grader.
