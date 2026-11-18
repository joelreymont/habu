---
title: "Save the quotation facts a signed word's signature states"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T10:24:26.529260+01:00"
closed-at: "2026-11-18T11:24:41.967709+01:00"
---

Problem: a join in a body unions facts into the parameter's declared term (src/core/checker.f QUOT-PAIR :4865) and a signed word is published from its live terms (CHECKER-PUBLISH-PARSED :16166-16170), so `: K2 ( bool [ -- ] -- ) dup KV ! swap if drop [: EVC ;] then RUN ;` is saved taking `[ -- renders ]` and `false ['] EVC K2` leaves EVC in KV `[ -- ]`. Acceptance: K2 is saved with its signature's facts and that call is rejected. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nD.
