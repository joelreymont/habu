---
title: "Write discovery's unreadable file as a record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.163578+01:00"
closed-at: "2026-11-16T04:07:33.775891+01:00"
---

Problem: tools/source-discovery.f DISCOVER:READ-THROW writes 'cannot read PATH' to fd 2 as a plain line, and its comments say refusal. Acceptance: one record through REJECTION, prose or JSON as asked; comments say reject. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
