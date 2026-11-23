---
title: Take str in script-required
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T16:47:39.637100+01:00"
closed-at: "2026-11-23T10:11:21.700826+01:00"
---

Problem: script-required (src/core/include.f:2830) takes ^u8 n because its generators (src/habu/main.f:161, tools/check-core.f:594,801) write unmarked text. Acceptance: it takes str, the generators write >str, its suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p2scr
