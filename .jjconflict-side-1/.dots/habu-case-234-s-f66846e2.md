---
title: "Case 234's child budget has 11 percent headroom"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T15:52:11.878785+02:00"
---

test/compiler/native-match.f case 234 spawns bin/hb --load test/compiler/native-match.f -- forge; the child re-runs the whole suite in 26.7s against CHILD-MS 30000 - 3.3s headroom on this host, and it reds (SIGKILL 137, expected 85) under concurrent lane load; pristine master reds identically with six forge children alongside (measured both ways by trusted-2, 2026-08-19). Same class as the insn-proof standalone timeout (b9e92813): a time budget calibrated on another host. Fix shape: the child should run only the forge leg, not the whole suite - or the budget derives from a measured floor like judge-timed's band. Costs lanes an hour each until fixed.
