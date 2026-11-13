---
title: "Close the reserved-cell definer's integer-to-pointer door"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-22T09:38:23.405799+03:00"
closed-at: "2026-11-10T21:50:20.733113+02:00"
close-reason: merged into habu-move-engine-internal-fc2ba551
---

RESERVED-PTR-U8-CELL (src/core/pointer-storage.f) is public and takes any offset, so a checked program mints a ptr ptr u8 cell anywhere in DATA. Measured on the A1 engine: '$3800 RESERVED-PTR-U8-CELL N : FORGE ( -- ) s" x" drop N ! ;' overwrites the engine's permanently-zero NULL-PTR cell, after which NULL-PTR ( -- ptr a ) hands that byte pointer back as a pointer of any pointee; and 'V NULL-PTR - data-base NULL-PTR - - RESERVED-PTR-U8-CELL X : AS-PTR ( -- ptr u8 ) X @ ;' returns an integer stored in variable V as a ptr u8. Both certify rc 0; master behaves the same. This is the launder habu-refuse-a-scalar-030be3ad and habu-refuse-ptr-field-331a9731 close for views and ptr-field, reached by declaration; their design left it to the system-package work (habu-move-engine-internal-fc2ba551, docs/architecture.md Packages and privacy). Acceptance: checked code outside the engine cannot declare a reserved cell at an arbitrary offset; the engine's own reserved cells (layout.f, include.f, check-hook.f, engine-origin*.f) keep working; the tools that read SRCLOC:PATH-CELL (tools/native-unit-dependency.f:335, tools/native-unit-object.f:84) get a named engine word for that cell instead; the forge probes above are refusal rows in test/compiler/raw-cell-pointer-refusals.f. Depends: habu-move-engine-internal-fc2ba551.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.
