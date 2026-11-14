---
title: Delete what the text deletions left
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T21:19:15.359624+01:00"
---

Problem: src/core/include.f INCLUDE-EVALUATE has no caller outside tests, and sumtype.f TDGEN-BUF is 16 KiB with a 'generated constructor too long' die though it holds one name. Acceptance: INCLUDE-EVALUATE and its rows and doc lines are gone, its tests use evaluate-closed; TDGEN-BUF is sized by the names it holds with no die a declaration cannot reach. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-dead.
