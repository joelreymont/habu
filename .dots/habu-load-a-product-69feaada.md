---
title: Load a PRODUCT of 256 fields
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T18:16:28.857830+01:00"
---

Problem: `PRODUCT revp256 FIELD f1 n ... FIELD f256 n ;PRODUCT` (256 one-cell fields, a 3.2 KB body) is rejected with "declaration failed at 'revp256'", naming no cause; 255 fields load (found by the review of pyyswxsn, on its engine). Acceptance: a PRODUCT of 256 or more fields loads and answers its fields, as a type-decl test; if a bound turns out to be inherent, the rejection names it and the fix instead. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-p256.
