---
title: Keep only the engine data a program reaches
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:46:47.574947+01:00"
---

Problem: a stripped program keeps the engine's whole header unit, [BASE, OPEN-DP) of DATA (src/habu/link-data.f), whatever it reaches: 2,876,656 bytes of address space, about 450 KB of them non-zero in Zen's server. Acceptance: on both machines a stripped program carries the engine's data its kept code and data reach and no more, and runs as before. Depends: none. Claim: unassigned.
