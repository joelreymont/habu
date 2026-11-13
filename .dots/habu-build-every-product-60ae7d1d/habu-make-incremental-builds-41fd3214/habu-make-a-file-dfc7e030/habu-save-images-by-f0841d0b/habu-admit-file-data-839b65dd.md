---
title: Admit FILE data by declared type
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:06.011376+02:00"
closed-at: "2026-11-04T11:48:47.903903+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the file linker (docs/design.md \"Machinery being replaced\"). tools/native-unit-dependency.f POINTER-CELL?/MAPPED? admission of per-definition FILE data; goes with the definition cache."
---

T4 (plan section T4): tools/native-unit-dependency.f:630-633 drops the raw variable arm of POINTER-CELL?, :776-805 MAPPED? becomes 'not DATA, region or 0'; delete AOT-CELL:MAPPED? and src/habu/proc-maps.f if unused. Verify: native-file-admission-e2e, native unit suites, full suite. Depends: T2. Claim: unassigned.
