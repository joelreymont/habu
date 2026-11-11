---
title: Pre-pass reads a target predicate by binding
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:27:50.781698+02:00"
---

Problem: src/habu/verify-source.f TARGET-GUARD (911) reads the six target predicates by spelling, with no shadow tracking: a file that defines one (the load admits it and calls reach it) is read by the pre-pass as the engine's predicate. Loader names already have the rule (NAME-DECLARED 704, QUIET-USE 725, E-DISC-SHADOW), quiet mode only (hunt 633076da). Acceptance: the pre-pass refuses or follows a file's own definition of a target predicate the way it does a loader name, in every composition (not quiet-only), and check's reserved-name lint no longer needs its target-predicate rule (reserved-name-lint-core.f 209-215). Files: src/habu/verify-source.f. Verify: a file defining one predicate then using it under check and --verify-only gives the load's answer. Depends: none. Ownership: verify-source.f TARGET-GUARD. Claim: unassigned.
