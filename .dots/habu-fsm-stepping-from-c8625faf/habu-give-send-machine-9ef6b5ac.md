---
title: Give SEND-MACHINE row-specific inputs
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T19:10:47.116332+02:00"
---

Problem: src/lib/net/model.f:3367 SEND-MACHINE uses one send-event enum for both lookup (missed, replayed, faulted) and dispatch (armed, faulted), so each state carries rows its SENSE never returns. `IN state SENSE ( row-inputs )` already lets a state declare its own inputs.
Acceptance: lookup and dispatch each sense their own result enum, constructors narrowed with the table; the three cross-phase rows are gone; the `lost` final is gone if nothing reaches it; error handling, journal writes, cancellation and cleanup behave as before.
Files: src/lib/net/model.f.
Verify: the model client's focused tests driving the send and fault paths pass; the row count before and after is recorded in this dot (before number written before dispatch, after number at close).
Depends: none.
Ownership: src/lib/net/model.f. Claim: unassigned.
