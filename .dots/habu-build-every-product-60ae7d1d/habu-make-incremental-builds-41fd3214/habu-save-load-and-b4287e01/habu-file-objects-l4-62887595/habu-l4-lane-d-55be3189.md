---
title: "L4 lane D: Link an object's data"
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T01:29:13.909360+02:00"
---

Problem: L4 needs LINK-DATA and PUT-CELL in linker.f: allot and copy D at FILE-DP, cell rows through xt!, ptr-cell! and scratch-cell-mark, strings through INTERN:ADOPT, STORES rows replayed. Brief .jj-ws.noindex/carl-l4ld/.out/brief-l4-data.md. Verify: the four suites (TFAIL = carl-l4/.out/l4i-suites.log); W runs it end to end. Acceptance: the lane's brief items done as written, a code-reviewer ACCEPT, merged into .jj-ws.noindex/carl-l4. Depends: lane R (ksuswurv). Ownership: worker lane. Claim: agent=worker workspace=.jj-ws.noindex/carl-l4ld (review ACCEPT; lead fold: per-row GOOD? checks, casts >XT and >XT-CELL; merged into carl-l4 rwyzklot; fold re-review ACCEPT)
