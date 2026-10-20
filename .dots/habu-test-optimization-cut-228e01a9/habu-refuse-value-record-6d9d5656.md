---
title: Refuse value-record names in ENUM and STRUCTURE
status: closed
priority: 2
issue-type: task
created-at: "2026-10-02T00:07:43.882021+02:00"
closed-at: "2026-10-14T19:30:56.944933+03:00"
close-reason: "Implemented in 2d4c89b0: family definers share TYPE-NAME:FAMILY-RESERVED?, so ENUM and STRUCTURE refuse value-record names in loader and check.f; failing-before fixtures, suites and convergence passed."
---

Problem: ENUM-DECL and STRUCTURE-DECL NAME-RESERVED? (src/core/enum-decl.f:212, src/core/structure-decl.f:189) lack the VREC-FIND arm that TDECL-RESERVED? (src/core/sumtype.f:204-206) and type-family.f:2499 have. `VALUE-RECORD vr ... END-VALUE-RECORD` then `ENUM vr ...` fails late at top level and loads inside a package, so one name means two kinds. Found by enumptr's lane (198). Acceptance: both definers refuse a value-record name with the same "reserved name" refusal as the legacy definers, at top level and in a package; loader and tools/check.f agree; the rows fail first on the parent engine. Prefer one shared predicate over a third copy of the list. Files: src/core/enum-decl.f, src/core/structure-decl.f, src/core/sumtype.f (read), tools/check-test-lib.f or the declaration suites. Verify: baked files: rebuild, g1 = g2 with .names, two-generation build, focused declaration tests.
