---
title: Check a built call against the source policy
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T17:48:43.750423+01:00"
closed-at: "2026-11-14T19:06:02.579135+01:00"
---

Problem: a text body token meets the sealed-source policy (src/habu/interpreter.f BODY-POLICY, SEARCH: LOOKUP-NAME then POLICY-CHECK-TOP), but a definer's CALL item (ITEM-RUN's CALL case) is found by LOOKUP-NAME alone, so a sealed source that calls a definer (FSM:DEFINE with an ACTION name) reaches words the policy refuses to its text. Acceptance: a sealed source's definer-built call of a word the policy refuses is rejected as the same name in its text is, as a test. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-fpol.
