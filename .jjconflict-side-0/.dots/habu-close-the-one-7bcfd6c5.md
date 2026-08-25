---
title: Close the one-address memory recurrence
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T07:21:39.137006+02:00"
---

STORE-LOAD: 24 gap bytes / 1.07 ns (re-measured 2026-08-10) - a loop whose body's only memory access is through one address (store then load back each turn) closes to 3*len arithmetic, but the fold needs the 'only access in the body is through ONE address' argument, which is an aliasing claim the affine-accumulator rule (habu-close-the-loops-1571fb6f Shape A) never makes. Build it as a second recognizer in the same HIR pass once Shape A lands, with the aliasing precondition derived structurally (single base, no other memory ops, token chain linear) and refused otherwise. Bit-for-bit incl. trip 0/1/MAX. Depends: habu-close-the-loops-1571fb6f.
