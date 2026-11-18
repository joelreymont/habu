---
title: "Reject a cell declaration outside the build's data"
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T13:41:29.073746+01:00"
---

Problem: ptr-cell-mark, ptr-cell!, xt! and scratch-cell-mark take any address, and one outside the data the build allotted stops the kernel instead of being rejected: `: Q ( -- ) 4096 map-anon drop {: p:^ :} p ptr-cell-mark ; Q`, and the same with `0 p ptr-cell!`, exit with E-XTBAND 'snapshot address cell out of range' (src/habu/habu2.f LPTRMARK) on the installed hb. Acceptance: the checker rejects a program that can hand a cell declaration an address outside the build's data, under tools/check.f -- --verify-only and when it runs, and the kernel's range check on the declaration is deleted. Depends: none. Claim: agent=jerry workspaces=.jj-ws.noindex/jerry-d1 (data-base rows), jerry-dt1 to jerry-dt8 (its tests).
