---
title: "Say the Design's help for a body that ends wrong"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:34:21.139855+01:00"
closed-at: "2026-11-16T04:07:34.108266+01:00"
---

Problem: END-HELP (render.f ~1675) says 'add the missing values after drop so the body ends with n, or declare ( n -- )'; the campaign Design's help is 'leave an n after drop, or declare ( n -- )'. A does> clause's message names the definer lower-cased: DOES-NAME! (check-walk.f:2212) keeps only the folded name. Acceptance: the help reads as the Design's; the does> message spells the definer as written. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
