---
title: Document stepping, finals and callback contracts
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T19:10:47.103778+02:00"
blocks:
  - habu-generate-name-step-52f37768
---

Problem: docs/forth.md:806 State machines describes only the runner and the row words. Nothing states when a terminal action runs, that a FINAL is a normal outcome rather than a `finally`, that SENSE may do work, or that --verify-only's parses-through defers the checks of generated words.
Acceptance: docs/forth.md, the docs/forth-card.md FSM entry and the src/lib/fsm.f header state the generated words and their effects. forth.md carries: a complete synchronous scanner; a complete event-fed STEP machine whose caller owns the phase and stores it after a successful step; the timing table (what runs on each kind of step and on a full run); an expected-failure classifier that rethrows other codes; a scoped-resource example; a row-specific enum example; the distinction between transient phase, owner state and stored resume phase; one-cell ctx, compact enums, real-load checking and the nonreturning-callback rules; the --verify-only caveat. A failed generated definition names the machine and, where known, the state, input, role and line; check the current error path first and extend it only where it falls short.
Files: docs/forth.md, docs/forth-card.md, src/lib/fsm.f header.
Verify: every documented example loads as written under bin/hb from a scratch file; one deliberately wrong SENSE effect yields a diagnostic naming the machine and the state.
Depends: Generate NAME-STEP beside the runner.
Ownership: docs/forth.md, docs/forth-card.md, src/lib/fsm.f header comment. Claim: unassigned.
