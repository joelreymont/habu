---
title: Give SEND-MACHINE row-specific inputs
status: closed
priority: 3
issue-type: task
created-at: "2026-11-09T19:10:47.116332+02:00"
closed-at: "2026-11-09T21:34:45.642457+02:00"
close-reason: landed; model-test.f 18511 checks rc 0 and test-model-test rc 0; Opus review accepted (removed lines and lost unreachable)
---

Problem: src/lib/net/model.f:3770 SEND-MACHINE uses one send-event enum for both lookup (missed, replayed, faulted) and dispatch (armed, faulted), so each state carries rows its SENSE never returns. `IN state SENSE ( row-inputs )` already lets a state declare its own inputs.
Acceptance: lookup and dispatch each sense their own result enum, constructors narrowed with the table; the three cross-phase rows are gone; the `lost` final is gone if nothing reaches it; error handling, journal writes, cancellation and cleanup behave as before.
Files: src/lib/net/model.f.
Verify: the model client's focused tests driving the send and fault paths pass; the row count before and after is recorded in this dot (before number written before dispatch, after number at close).
Depends: none.
Ownership: src/lib/net/model.f. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-send.

Before (master 3d6257a9): src/lib/net/model.f:3769-3784 SEND-MACHINE has 2 IN rows with 8 lines (3 only cross-phase: looking armed, sending missed and replayed) and 3 FINAL rows (out, ended, lost).
After (xnklptsn): src/lib/net/model.f:3770-3784 SEND-MACHINE has 2 IN rows with 5 lines (looking senses lookup-event, sending senses dispatch-event; the 3 cross-phase lines and the unreachable `lost` final are gone) and 2 FINAL rows (out, ended); model-test 18511 checks, rc 0.
