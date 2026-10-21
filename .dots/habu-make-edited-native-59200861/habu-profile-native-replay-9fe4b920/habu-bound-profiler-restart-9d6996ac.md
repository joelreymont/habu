---
title: Bound profiler restart work
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T12:53:45.237623+03:00"
closed-at: "2026-10-21T13:29:22.807853+03:00"
close-reason: "Old120s reset timeout no longer reproduces: current baseline gate debug8.506s, final matching profiler9.218s under load, focused gate7.57s. Existing reset/prof-index/rewind paths pass without deadline changes. No additional reset mechanism needed."
---

Matching privatea7c8b6f0 profiler rewind and2380index probes pass, but gate-debug GDB-PROFILER-RESETS times out after120000ms with no output. Measure CPU and reduce repeated prof-on/reset behavior; distinguish host load from added name-copy/startup work, then fix any demonstrated regression at the profiler owner boundary without raising deadlines or adding another profiler framework. Existing real child is the regression.
