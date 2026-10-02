---
title: End every fatal engine message with a newline
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T23:12:52.737467+02:00"
---

Problem (lane 367 r4-bootnl, 7d7ea7a9): these fixed fatal messages are written by exact length and end without a newline: habu1.f:1349 'hb: dictionary count out of range', habu1.f:2824 and habu2.f:10784 'hb: catch frame corrupt', habu1.f:3296 'hb: protected-WID id above the bound', habu1.f:4341 'hb: dictionary index alloc failed', habu1.f:4371 'hb: dictionary index exhausted', rt.f:55 'hb: stack bounds exceeded'; kernel-x64.f:143 matches them on purpose. Acceptance: every fixed engine diagnostic ends with LF on both kernels (single S\" ...\n" literals, length from the literal), tests that pin them updated to exact bytes, rebuild, g1 == g2, two-gen. Files: src/habu/habu1.f, habu2.f, rt.f, kernel-x64.f.
