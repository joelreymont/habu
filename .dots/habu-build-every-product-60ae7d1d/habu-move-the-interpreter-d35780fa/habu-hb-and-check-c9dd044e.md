---
title: "hb and check.f disagree on a bad FUNCTION:'s first fault"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:35:28.165254+02:00"
---

Problem: hb --load and tools/check.f refuse a bad `FUNCTION:` (the foreign-function declaration, src/lib/ffi-abi.f:1672) for different faults first. Reproducer: with no library selected, `require src/lib/ffi-abi.f` then `FUNCTION: CKF getpid` under bin/hb --load ends with throw -3503 (E-FFI-LIBRARY, raised by CHECK-SCOPE, the first step of OPEN-FUNCTION at ffi-abi.f:1540-1548); with a library selected and no signature, -3501 (E-FFI-SYNTAX, from EFFECT). tools/check.f refuses the same source first as a missing signature (diagnostic 7157 at `FUNCTION:`). That the throw prints as a bare number is habu-uncaught-throws-name-1a43b055's. Which order is right is not settled. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces.
Acceptance: the order in which a `FUNCTION:` declaration's faults are reported is decided and written beside OPEN-FUNCTION; then hb --load and tools/check.f report the same fault first for the reproducer.
Depends: none.
Claim: unassigned.
