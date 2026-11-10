---
title: DISPOSE-OBJECTS leaves OWNED and OWNED-TEXT mapped
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T21:01:24.076583+02:00"
---

Problem: src/habu/linker.f DISPOSE-OBJECTS releases every DYNAMIC-BUFFER of the linker but OWNED and OWNED-TEXT (declared near the file's top), so each build in one process leaves their mappings (feature review 2, by comparing the declarations with the release list; the base revision f44c58c9 has it too). Acceptance: DISPOSE-OBJECTS releases both. Files: src/habu/linker.f. Verify: file-objects passes (its build.f runs many builds in one process). Depends: none. Ownership: unassigned. Claim: unassigned.
