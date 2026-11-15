---
title: "Re-pin the suite's tests to rejection records"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T20:07:41.833067+01:00"
closed-at: "2026-11-16T04:07:34.333888+01:00"
---

Problem: about 60 tests assert the old diagnostic forms (habu: in F lines, 71xx codes, removed JSON keys), so the suite on the C4 integration fails where the engine is right, and a real regression (the exit-hook link failure) hides among them. Acceptance: every listed test passes on the integration engine, or an assert left failing names an engine defect with its own fix. Rules: /tmp/jerry-c4/bt-common.md. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
