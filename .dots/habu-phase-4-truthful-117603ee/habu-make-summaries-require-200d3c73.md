---
title: Make summaries require full workload completeness
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.351873+02:00"
blocks:
  - habu-enforce-payload-schema-d6f7f109
---

Problem: summaries can be emitted from biased subsets or dirty workload rows. Acceptance: geomeans, hotspot reports, rankings, and recommendations require full declared workload completeness or are explicitly demoted. Files: tools/bench_pack_runner.py, tools/perf-loop, tools/comprehensive-bench, tools/maxima-hotspots. Verify: partial workload payloads are rejected by authoritative paths. Blockers: habu-enforce-payload-schema-d6f7f109.
