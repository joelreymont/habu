---
title: Phase 3 canonical correctness closure
status: open
priority: 2
issue-type: task
created-at: "2026-04-01T22:06:02.026292+02:00"
blocks:
  - habu-phase-2-make-db0483f9
---

Problem: make test-batch the only authority and reduce remaining failures to concrete Habu gaps. Acceptance: Phase 3 leaves cover PLAN.md 3.1-3.5. Files: PLAN.md:465-532, ../maxima/tests/**, ../maxima/share/**, tools/maxima-rtest.lisp. Verify: dot tree habu-exec-converged-clean-536008dd. Blockers: habu-phase-2-make-db0483f9.
