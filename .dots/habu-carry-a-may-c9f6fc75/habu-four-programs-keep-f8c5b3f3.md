---
title: Four programs keep a quotation in a slot that lacks its facts
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T11:24:58.781881+01:00"
---

Problem: with `: EVC ( -- ) s" 1 drop" evaluate-closed ;` and `TYPED-VARIABLE KV [ -- ]`, each of these is accepted and puts EVC (renders) into KV. A returned parameter, `: PASS ( [ -- ] -- [ -- ] ) ;` then `['] EVC PASS KV !`: a returned parameter is not judged kept (src/core/checker.f KEEP-INNER skips the top-level outputs). A RECURSE argument, `: R ( bool [ -- ] -- ) swap if drop false [: EVC ;] RECURSE else KV ! then ;`: the instance shares the block variable (OWNB-SHARE) but is judged before `KV !` raises it. A loop back edge, `: W ( [ -- ] n -- ) 0 ?do dup KV ! drop [: EVC ;] loop drop ;`. An unsigned join, `: U dup KV ! [: EVC ;] SAME ;` with `: SAME ( a a -- ) 2drop ;`, then `['] EVC U`. The last two share a cause: a join writes into a term a keeping was already judged on (OWNQ-RESTORE covers only a signed word's publish). Acceptance: each is rejected at its keeping; the xt-effect suite passes. Depends: none. Claim: unassigned.
