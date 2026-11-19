---
title: Declare a long by-value chain of families
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:57:24.505975+01:00"
---

Problem: 5000 structures each holding the previous by value (/tmp/deepcut-probe/chd5000.f) stop with exit 102, data stack, in TFW-FAM (src/core/type-family.f); 4000 take 2.3 s. Not traced: TFM-EPOCH (type-family.f:570) is bumped by every declaration, so the width memo is void at each family and the chain is walked again. Acceptance: the chain at 5000 and 20000 families declares in time linear in the count, or is rejected at the family that passes a limit derived from the stacks; a test at the old failing count. Depends: none. Claim: unassigned.
