---
title: "A refused definition's remaining tokens cost time quadratic in their count"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:15:08.227642+02:00"
---

Problem: while a refusal's pin is held COMPRESS? is off, so chains of row variables grow with every later token: a 'then' refusal over an empty stack followed by 40,000 tokens takes 16.3 s (4,000: 0.8 s) on master; habu-a-refusal-s-4e54728f's commit extends this to every refused kind and to deferrals (an underflow at token 10 of 40,000: 2.5 s before, 16.2 s after; --verify-only with an unseen name 2.7 to 10.4 s). Programs /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/pin-review/time. Found by the review of habu-a-refusal-s-4e54728f (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/pin-rows-review.md, finding 3; cause inferred from scaling, not traced). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a measurement names the cause; then the time after a refusal grows linearly with the tokens left, with the pinned rows still true; a timing case bounds it. Files: src/core/checker.f. Verify: time/uf40k.f and thenempty40k.f before and after. Depends: the design for habu-a-refusal-s-4e54728f. Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-refused-def-fe5ca1f0

Design: /Users/joel/Work/reviews/habu-10x-2026-11-10/designs/rows-design.md lane 1 (every compression store goes on the trail, COMPRESS? is deleted); brief /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Trail-every-compression-store.md, which confirms the inferred cause before editing.

Revised 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/trail-stop.md): the cause is confirmed (thenempty40k.f 16.5 s, 2.43 s with the pin term gone). With compression never switched off, accepted code checked about 4% slower (ok40k.f 2.72 against 2.62 user s), because a failed trial undoes what it compressed. Settled by the long-term rule: every compression store is trailed, and compression is still skipped inside a trial and a held quotation comparison, as a cost rule and no longer a correctness rule; a pinned refusal no longer switches it off. Brief: /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Finish-trail-with-trial-guard.md.
