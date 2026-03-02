---
title: Enforce payload schema and provenance
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.345877+02:00"
blocks:
  - habu-hermeticize-perf-executables-8b014aaa
---

Problem: helper payloads and comparator inputs are not strictly validated before publication. Acceptance: schema, version, producer identity, command success, and provenance are mandatory and malformed payloads are fatal. Files: tools/gc-compare, bench/check.zig, tools/bench_pack_runner.py, tools/comprehensive-bench, tools/bench_compare.sh. Verify: malformed and spoofed payload tests fail closed. Blockers: habu-hermeticize-perf-executables-8b014aaa.
