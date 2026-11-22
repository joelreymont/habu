---
title: "Say str in REQUIRE-REG:APPEND"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T16:47:39.666234+01:00"
---

Problem: REQUIRE-REG:APPEND (src/core/include.f:1229) takes ^u8 n; the build's end.f calls it from unmarked text written by the host's embedded builder, so it moves in two installs. Acceptance: stage 1, end.f starts with split-str, installed; stage 2, the row says str, the builder writes >str and drops the line. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p2req1
