---
title: Redesign native gate architecture
status: active
priority: 1
issue-type: task
created-at: "\"\\\"2026-06-29T21:46:42.001042+02:00\\\"\""
---

Problem: hot zed native gate is 111271ms with warm-miss=0 because test/run.f still schedules one process per phase and test/gate-stdlib-lib.f TEST-SUITE/TEST-TOOL-SUITE still spawn child hb for semantic tests. Evidence: tool-boundary=78814ms, tail=44055ms, dictionary=43129ms, check-cli=27085ms, lint-tools=25596ms, helper-spawn=82, inner-hb=56, inner-hb-stdin=17. Fix: redesign gate around explicit subject lanes (host-source, candidate-cli, candidate-source, artifact), span metrics/critical path, semantic in-process suites, small CLI-contract sentinels, and batched candidate probes. Files: test/run.f, test/gate-stats.f, test/gate-stdlib-lib.f, test/gate-stdlib-inline-lib.f, test/gate-stdlib-cases.f, test/gate-common-lib.f, tools/*-test.f, docs/critical-path.md. Verify: focused slices show labeled span/subject output; zed hot gate reports critical path and cuts duplicated helper spawns without losing CLI boundary rows.
