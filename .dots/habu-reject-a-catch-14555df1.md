---
title: Reject a catch over a row-variable quotation by place
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T13:55:53.125395+01:00"
---

Problem: `: SAFE3 ( R [ R -- R ] -- n ) catch ;` makes the installed hb die with the bare line "checker: stale type in stored effect", exit 76 (src/core/checker.f:11258, EXIT-STATUS:INTERNAL); tools/check.f reports E-VERIFIER-ENDED. A user's definition reaches an internal-error exit with no place, line, expected/found or fix. Acceptance: the definition is accepted or rejected with a located diagnostic, never exit 76, by the hunt's cause. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-safe3
