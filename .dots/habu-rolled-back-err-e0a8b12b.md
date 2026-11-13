---
title: Rolled-back error codes and FFI rows leave heads on reclaimed storage
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T16:44:37.179027+01:00"
---

Problem: a definition inside a caught evaluate that throws is rolled back and its storage reused, but heads and counts below the cut keep naming it: src/lib/error.f HEAD (:723, stored by ROW, :845-861; run: ERROR:NAME$ faults walking into the next definition), src/lib/ffi-abi.f FN-TABLE/LIB-TABLE with FN-N/LIB-N (:197-243, :618, :762), src/lib/ffi-callback.f TABLE (:62, :111) and src/lib/task.f ROW-TABLE/ROW-CAP/ROW-N with the published CB-ROWS cell (:850-870). Only the error case was run (reproducer /tmp/carl-rollback/error-code.f on spark). Acceptance: each is marked by ROLLBACK-CELLS:MARK (src/core/rollback-cells.f), and a test beside each module holds a rolled-back definition followed by a read. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-rbheads1 (error, FFI), .jj-ws.noindex/carl-rbheads2 (callbacks, task rows).
