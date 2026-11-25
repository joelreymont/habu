---
title: "Meet a kept quotation's facts around a loop"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:16:04.236697+01:00"
---

Problem: a loop drops the facts of a quotation its body replaces: ': USE ( n -- ) [: ;] swap 0 ?do dup KV ! drop [: parse-name drop ;] loop drop ;' loads with rc 0, with KV a TYPED-VARIABLE of [ -- ], although the second pass stores a quotation that parses; the facts of the loop body's last state are never met with those it read at its start. Acceptance: load and check reject USE, and a diag-position case fails without it. Depends: none. Claim: unassigned.
