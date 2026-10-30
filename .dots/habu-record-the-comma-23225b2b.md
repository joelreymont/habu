---
title: Record the comma run on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:17:43.054125+02:00"
---

Problem: the comma lane records the run of comma cells in the arm64 engine (habu1.f BCOMMA, DP-LOWER, ADDR-TAIL) and attaches it as the created body grid; src/habu/kernel-x64.f's comma (:3617-3620) and its create never write the run cells, so on Linux x86-64 no comma grid forms and a stripped build that straddles comma cells is still refused (comma lane 2 review, finding 4; docs say ARM64 until this lands). Acceptance: kernel-x64.f writes the same three run cells at the same points as the arm64 kernel; test/cell-body-e2e.f and cell-body-image-e2e.f pass on the x86-64 host (joel@pulsar-ua-106); forth.md and forth-card.md drop the ARM64 qualifier. Files: src/habu/kernel-x64.f, docs/forth.md, docs/forth-card.md. Verify: the two tests on the x86-64 host; two-generation convergence there. Depends: the comma lane landed. Ownership: src/habu/kernel-x64.f. Claim: unassigned.
