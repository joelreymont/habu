---
title: "Check CAST: owner rules on foreign pointees evenly"
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T12:31:54.196358+02:00"
---

Finding (census tc1, 2026-10-31): in package XML-C2, CAST: ( ptr u8 -- ptr XML:cursor ), a pointer to another package's STRUCTURE, is admitted, while CAST: ( ptr n -- ptr NUM:alloc-byte-len ) is refused E-CAST-OWNER. Either the owner check misses the first or wrongly refuses the second. Acceptance: one stated rule for a cast whose result names another package's type, both cases following it, with rows in the cast tests. Files: src/core/checker.f cast owner check. Evidence: /tmp/jerry-big/trusted-census-tc1.md, /tmp/jerry-big/tc1-c2-mem-probe.diff. Ownership: unassigned.
