---
title: x86-64 , keeps no comma run
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T14:34:49.712849+02:00"
---

Problem: arm64's `,` (src/habu/habu1.f BCOMMA :2011-2018) keeps the latest run of cells written at a created body in CREATED-BODY-ABI:COMMA-RUN-CELL and COMMA-END-CELL, and the arm64 kernel ends the run where DP lowers (:1998); x86-64's `,` (src/habu/kernel-x64.f :3630-3633) and its create write none of the run cells, so on x86-64 src/core/checker.f CHECKER-BODY-CLOSE (:16419-16435) never sees a run starting at COMMA-BASE and never attaches a created body's `,` storage grid. A machine asymmetry in the one typed interpreter's primitives. Acceptance: kernel-x64.f writes the same three run cells at the same points as the arm64 kernel; a checker case whose created body's `,` grid refuses a bad read behaves the same on arm64 and x86-64 (pulsar). Depends: none. Claim: unassigned.
