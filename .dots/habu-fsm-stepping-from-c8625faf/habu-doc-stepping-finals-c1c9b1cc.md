---
title: Document stepping, finals and callback contracts
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T19:10:47.103778+02:00"
blocks:
---

Problem: docs/forth.md "State machines" (801-887) now states the generated words, what runs on each step and on a full run, and the --verify-only caveat (fd307801). docs/forth-card.md names FSM:DEFINE only in a list (585), and no document carries a complete event-fed machine a consumer can copy.
Acceptance: the card's FSM entry states the generated words and their effects (NAME, NAME-STEP, NAME/state, NAME/final). forth.md carries one complete event-fed NAME-STEP machine whose caller owns the phase and stores it after a successful step. A wrong SENSE effect yields a diagnostic naming the machine and the state: check the current error path first and extend it only where it falls short. The failure classifier, scoped-resource and resume-phase examples the plan listed are consumer policy and stay out of Habu's docs.
Files: docs/forth-card.md, docs/forth.md.
Verify: the example loads as written under bin/hb from a scratch file; one deliberately wrong SENSE effect yields a diagnostic naming the machine and the state.
Depends: none (NAME-STEP landed in fd307801).
Ownership: docs/forth-card.md, docs/forth.md. Claim: unassigned.
