---
title: Adopt canonical test-batch/run_testsuite execution path
status: active
priority: 1
issue-type: task
created-at: "\"2026-03-07T19:20:07.534362+01:00\""
blocks:
  - habu-finish-composite-stream-638bd128
---

tools/maxima-rtest.lisp:1-64; ../maxima/src/mload.lisp:379-509,817-916. Root cause: the custom runner is useful for triage but not authoritative. Fix: make test-batch/run_testsuite the canonical parity gate and demote the custom runner to non-authoritative triage unless matched semantically. Why: correctness measurement must use Maxima’s own comparison/error semantics.
