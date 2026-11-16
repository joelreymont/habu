---
title: Throw when the address-cell table cannot grow
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:09:22.135509+01:00"
closed-at: "2026-11-16T13:54:13.971119+01:00"
---

Problem: src/habu/habu2.f EMIT-MARK dies `E-XTCELL habu: address-cell storage allocation failed` (rc 96) with its lock held when the system refuses the address-cell table's grow, and src/habu/address-cells.f :61 dies 96 for an engine defect. Acceptance: a refused grow throws a named error.f code, silent when caught and printed as that code's record when not; the defect dies 76. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w221.
