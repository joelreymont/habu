---
title: Make test-batch sole authority
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.260550+02:00"
blocks:
  - habu-make-maxima-rtest-f2324a92
---

Problem: correctness still has competing ad hoc authorities. Acceptance: canonical test-batch path is the only authoritative runner and fails closed on partial loader state or non-success outcomes. Files: ../maxima/src/mload.lisp:379-509, tools/maxima-rtest.lisp, custom runner glue. Verify: automation uses test-batch and refuses partial-state success. Blockers: habu-make-maxima-rtest-f2324a92.
