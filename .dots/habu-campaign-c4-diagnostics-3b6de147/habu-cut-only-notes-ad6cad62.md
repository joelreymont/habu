---
title: Cut only notes that quote program text
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:02:34.782247+01:00"
closed-at: "2026-11-16T04:07:36.548977+01:00"
---

Problem: src/core/rejection.f END-TEXT cuts every prose note past 137 bytes, so a `= searched:` path (test/source-root-exe-test.f) and a `= word:` note lose what the agent must act on. Acceptance: a note shows in full unless its caller quotes program text through the cut; source-root-exe, name-length and signature-nesting pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
