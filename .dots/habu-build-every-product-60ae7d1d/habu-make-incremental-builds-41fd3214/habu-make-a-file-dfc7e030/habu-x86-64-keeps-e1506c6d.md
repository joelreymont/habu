---
title: x86-64 , keeps no comma run
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T14:34:49.712849+02:00"
---

Problem: arm64's `,` (src/habu/habu1.f BCOMMA :2199-2208) keeps the latest run of cells written at a created body in CREATED-BODY-ABI:COMMA-RUN-CELL and COMMA-END-CELL; x86-64's `,` (src/habu/kernel-x64.f :3634) writes neither, so on x86-64 src/core/checker.f CHECKER-BODY-CLOSE (:16641-16658) never sees a run starting at COMMA-BASE and never attaches a created body's `,` storage grid. A machine asymmetry in the one typed interpreter's primitives (DECISIONS.md "one compiler"). Found by the kinds-a review (2026-11-04), after the kinds-a fold makes definers.f DEF-ADDR-TAIL write COMMA-BASE-CELL on both machines. Acceptance: x86-64's `,` keeps the run as BCOMMA does; a checker case whose created body's `,` grid refuses a bad read behaves the same on arm64 and x86-64 (pulsar). Files: src/habu/kernel-x64.f. Verify: the case on both machines. Depends: the kinds-a fold landing (habu-compile-create-s-311edca9). Ownership: kernel-x64.f `,`. Claim: unassigned.

Acceptance case 2026-11-10 (bob; merged from habu-record-the-comma-23225b2b "Record the comma run on x86-64", one cause with this dot): Problem: the comma lane records the run of comma cells in the arm64 engine (habu1.f BCOMMA, DP-LOWER, ADDR-TAIL) and attaches it as the created body grid; src/habu/kernel-x64.f's comma (:3617-3620) and its create never write the run cells, so on Linux x86-64 no comma grid forms and a stripped build that straddles comma cells is still refused (comma lane 2 review, finding 4; docs say ARM64 until this lands). Acceptance: kernel-x64.f writes the same three run cells at the same points as the arm64 kernel; test/cell-body-e2e.f and cell-body-image-e2e.f pass on the x86-64 host (joel@pulsar-ua-106); forth.md and forth-card.md drop the ARM64 qualifier. Files: src/habu/kernel-x64.f, docs/forth.md, docs/forth-card.md. Verify: the two tests on the x86-64 host; two-generation convergence there. Depends: the comma lane landed. Ownership: src/habu/kernel-x64.f.
