---
title: Replay a sum inside its transaction
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:09:21.962344+01:00"
closed-at: "2026-11-16T04:07:34.319924+01:00"
---

Problem: sumtype.f TDECL-DEFSUM replays a sum outside TDECL-TXN-XT, unlike TDECL-SUMTYPE, so a swallowed rejection under multi-error may leave partial effects. Acceptance: the replay runs in the transaction; type-decl and load-reject-diag pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
