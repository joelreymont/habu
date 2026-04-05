---
title: Make testsuite sole authority
status: open
priority: 1
issue-type: task
created-at: "2026-04-05T11:37:31.514491+02:00"
blocks:
  - habu-make-maxima-rtest-e12c672e
---

Problem: PLAN.md 3.1 still needs a converged leaf that cuts over fully to upstream run_testsuite/test-batch semantics and demotes all custom comparison paths to triage-only. Acceptance: canonical correctness reporting comes only from upstream testsuite semantics, with package-local setup flows and explicit nonzero process failure. Files: PLAN.md:786-806, tools/maxima-rtest.lisp, ../maxima/src/testsuite.lisp, ../maxima/src/mload.lisp. Verify: canonical core/share/package-local suite invocations match the upstream pipeline and fail closed on error.
