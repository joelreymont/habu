---
title: Check whether SX-RFLOOR and XTCELL-RC share a numbering space
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:13:58.569398+02:00"
---

Problem: src/habu/layout.f defines 96 constant SX-RFLOOR beside 96 constant XTCELL-RC; if both index one table the rows collide. Acceptance: a finding: the two live in different spaces (stated beside the constants) or one is renumbered with every reader updated. Files: src/habu/layout.f and the readers rg finds. Verify: rg -n 'SX-RFLOOR|XTCELL-RC' src test; bin/hb --load test/run.f when a number changes. Depends: none. Ownership: src/habu/layout.f. Claim: unassigned.
