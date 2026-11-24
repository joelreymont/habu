---
title: "Delete HTTP:BODY-READER and the JSONL file cursor"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.488883+01:00"
closed-at: "2026-11-24T19:35:33.954835+01:00"
---

Problem: HTTP:BODY-READER (http-request.f:1032, storage http-arena.f:127) answers a JSON:reader; JSONL's file cursor (OPEN NEXT CLOSE READER cursor E-STATE E-LIMIT) has only test callers. Acceptance: both deleted with their tests moved to PARSE; SPLIT-LINE stays. Zen (test/server.f, test/comparison.f, test/support/*-peer.f) and Tender (tenderd/api.f) call BODY-READER; they move to HTTP:BODY$ and JSON:PARSE. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-jsonl (both halves).
