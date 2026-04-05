---
title: Run rtest6b successor slice
status: open
priority: 1
issue-type: task
created-at: "2026-04-05T11:37:31.477022+02:00"
blocks:
  - habu-fix-rtest6-callable-8b0a2a4f
---

Problem: PLAN.md 2.1c is still undotted: after rtest6 floors are lifted, the immediate successor core slice must be run before broad sweeps so new blockers are concrete and close to the current front door. Acceptance: canonical runner executes rtest6b and the next named successor slice, and every newly exposed failure is either fixed or emitted as a concrete follow-up dot. Files: PLAN.md:620-640, tools/maxima-rtest.lisp, ../maxima/tests/rtest6b.mac and immediate successor files. Verify: canonical reports for rtest6b plus successor slice with follow-up dots for any remaining failures.
