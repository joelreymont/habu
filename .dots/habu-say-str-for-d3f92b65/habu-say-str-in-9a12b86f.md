---
title: "Say str in REQUIRE-REG:APPEND"
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T16:47:39.666234+01:00"
---

Problem: REQUIRE-REG:APPEND (src/core/include.f:1229) takes ^u8 n because the build's end.f, which the host's embedded builder writes, is split-str. Acceptance: the row says str, the builder writes >str and no longer marks end.f split-str. Depends: none. Claim: unassigned.
