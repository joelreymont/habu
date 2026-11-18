---
title: Check a fetch after a narrow FFI result local
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:06:02.847639+01:00"
---

Problem: in ': S ( ^u8 -- n ) {: buf :} buf 1 GLA {: got:i32 :} buf CELL-VIEW @ ;' after 'PROCESS-SYMBOLS FUNCTION: GLA getloadavg ( ^u8 n -- i32 ) ;FUNCTION', tools/check.f defers the final @ to the run (W-CHECK-DEFERRED at it; 'a stretch or definition deferred to the run is not verified'): binding the i32 result to a local leaves the check unable to follow the cell view after it. Acceptance: that definition verifies with no deferral, a test holds it; the cause is reported first. Depends: none. Claim: unassigned.
