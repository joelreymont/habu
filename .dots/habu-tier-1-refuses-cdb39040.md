---
title: Tier 1 refuses a half-pair local bind without naming it
status: active
priority: 3
issue-type: task
created-at: "2026-10-24T00:35:39.447520+03:00"
---

'STRUCTURE icpair FIELD a n FIELD b n ;STRUCTURE  TRUSTED: U3 ... 1 2 ICPAIR:MAKE s" x" drop {: p q:icpair :} q' (q takes the ptr and the pair's top cell, p lies over half a pair): tier 0 refuses with 'hb: incomplete layout facts: U3' rc 75, tier 1 exits 67 'ncomp: cannot compile' without naming the word. The test/tier.f INCOMPLETE-BIND$ row is this shape. Found by jerry, 2026-10-24. Not yet worked.

Lane: workspace .jj-ws.noindex/jerry-halfpair (jerry). Master already names U1/U2 in the tier-1 refusal (ncomp: cannot compile U1, -8519); the lane adds the missing tier-1 rows.
