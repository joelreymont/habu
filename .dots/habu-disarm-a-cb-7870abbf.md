---
title: Disarm a callback stub when its accessor is refused
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T01:28:37.965316+02:00"
---

FFI-CB EMIT (src/lib/ffi-callback.f, EMIT) arms the pending stub (ARM-STUB, i.e. NDEFINITION:CALLBACK-STUB!), then GEN-NATIVE defines the NAME-ENTRY accessor, then reads STUB-ENTRY. If the accessor's `:` is refused before the compiler entry runs (duplicate definition, rc 78), nothing disarms: M-CB-SLOT stays armed and the next unrelated definition is sealed with that slot's C stub (its entry goes from 0 to a code address, then the slot returns to -1). Probe: `: DUPX-ENTRY ( -- n ) 1 ;` then `CALLBACK: DUPX ( -- ) ;CALLBACK` (rc 78), then `: LATER ( -- n ) 5 ;`. NCOMP and T0 behave the same; found by the A3a2 review (2026-10-29), older than T0. Fix at FFI-CB EMIT: drop the pending stub on any exit that does not reach STUB-ENTRY, which needs a disarm word reachable through the image-ABI callback cells (a third cell next to CALLBACK-STUB-CELL and CALLBACK-ENTRY-CELL, or a disarm argument). Test: the probe above in a CALLBACK: test, asserting LATER seals with no stub and the next CALLBACK: arms.
