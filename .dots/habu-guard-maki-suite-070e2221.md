---
title: Guard maki suite registry headroom
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T16:06:24.420910+02:00\""
---

After the bitmap lands, assert at the end of maki/test.f that protected-registry headroom stays above a named floor, so the next approach to the ceiling names itself instead of surfacing as an arbitrary file's enum failure. Pure Habu, no engine change. The two-line RUN-PASS room-trace instrumentation from the investigation is worth keeping as a tools/ capacity probe. Depends on the bitmap dot.

Claim: agent=makilane workspace=.jj-ws/habu-fix-maki-competitive-7dc29ec2
