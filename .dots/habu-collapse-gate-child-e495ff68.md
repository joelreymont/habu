---
title: Collapse gate child hb launches
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-29T21:02:03.461305+02:00\""
---

Problem: hot zed gate still takes 114150ms with warm-miss=0 because warm runners are launchers, not in-process harnesses. Evidence: test/run.f schedules 20 phases; test/gate-stdlib-lib.f TEST-SUITE calls SUITE-HB-RUN per label; test/gate-dictionary-lib.f uses GE-HB-RUN-STDIN dozens of times; final counters top-phase=20 runner-phase=18 inner-hb=56 inner-hb-stdin=41 helper-spawn=106. Fix: route non-CLI semantic tests through in-process evaluate/check helpers and keep child hb only for true process-boundary contracts: argv/env/cwd/stdin/exit/signal/tools CLI. Verify: focused dictionary/stdlib slices reduce inner-hb counts and full zed hot gate passes <=70000ms.
