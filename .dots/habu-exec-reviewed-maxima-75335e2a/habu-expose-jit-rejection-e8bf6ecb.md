---
title: Expose JIT rejection counters in hotspots output
status: open
priority: 2
issue-type: task
created-at: "2026-03-08T17:08:50.333798+01:00"
---

Files: src/interp/vm.zig:JitAdmStats, tools/maxima-hotspots. What: surface sk_caps/sk_opt/sk_key/sk_rest/sk_assert (currently hidden) in the hotspot report. Why: PLAN 7.5 requires full rejection visibility before ordering 8.1-8.4 by data. Verification: maxima-hotspots JSON/text output shows all tracked rejection counters on a Maxima workload run.
