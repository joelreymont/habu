---
title: Place a replayed duplicate at its name
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:38.781822+01:00"
closed-at: "2026-11-16T04:07:35.037827+01:00"
---

Problem: a taken derived word (HEAP's NAME-NEW, DYNAMIC-BUFFER's NAME-RESERVE, ARENA's NAME-NEW) is rejected with no place: src/core/check-walk.f REFUSE-DUPLICATE places at SOURCE-NAME$, which answers the name assembled in LBUF-GEN; and the verifier (src/habu/verify-source.f ARENA-WORD-FREE?) names the declared word instead of the taken one. Acceptance: under the three readers the record names the taken word and stands at the declared name. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
