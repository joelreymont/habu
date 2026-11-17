---
title: Map the code region within BL reach
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:55:05.514275+01:00"
---

Problem: src/habu/habu2.f:1502-1519 maps the code region at a hint without a fixed mapping, so when the hint is taken the kernel places it anywhere and boot dies E-BL-RANGE: a Maki image with OCCT libraries LD_PRELOADed fails 5 of 200 starts on spark (tim, /tmp/tim-blrange.md, reproducer there). Acceptance: the root cause is shown, and the reproducer passes 1000 of 1000 starts. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-bl1.
