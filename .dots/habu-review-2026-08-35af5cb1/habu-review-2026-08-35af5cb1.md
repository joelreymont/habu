---
title: "Review 2026-08-22: checker and src/core"
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.782794+02:00"
---

Problem: the src/core audit (master 5ee6beb2, read-only) found unsound axiom rows, fail-open paths, masked throws, fixed caps and ~3.7k lines of duplicated declaration machinery. Acceptance: every child closed or refuted with evidence. Files: src/core/. Verify: per child. Depends: none. Ownership: checker. Claim: unassigned.
