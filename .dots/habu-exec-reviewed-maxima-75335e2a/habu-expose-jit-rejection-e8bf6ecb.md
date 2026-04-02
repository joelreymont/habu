---
title: Expose JIT rejection counters in hotspots output
status: closed
priority: 2
issue-type: task
created-at: "\"2026-03-08T17:08:50.333798+01:00\""
closed-at: "2026-04-02T16:46:20.257554+02:00"
close-reason: done; hotspots now prints all jit admission counters, validated with mocked formatter payload and CLI help
---

Files: src/interp/vm.zig:JitAdmStats, tools/maxima-hotspots. What: surface sk_caps/sk_opt/sk_key/sk_rest/sk_assert (currently hidden) in the hotspot report. Why: PLAN 7.5 requires full rejection visibility before ordering 8.1-8.4 by data. Verification: maxima-hotspots JSON/text output shows all tracked rejection counters on a Maxima workload run.
