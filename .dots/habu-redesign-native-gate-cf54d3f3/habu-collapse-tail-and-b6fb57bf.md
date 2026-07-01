---
title: Collapse tail and lint slice bottlenecks
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-30T23:24:33.490508+02:00\""
---

Problem: Mac hot test suite still spends ~7s in stdlib lint artifacts/lint tools and ~7-10s in stdlib tail slices. These are mostly host-source semantic checks and should run resident with shared setup. Fix: profile per-test spans, move pure lint/doc/tail semantics in-process, isolate only filesystem/artifact/process-contract sentinels, and ensure setup is per suite/group not per test. Acceptance: each tail/lint slice under 5s on Mac hot profile and report names every test/group with timing.

Progress: macos-arm64-12x2 hot proof on 2026-06-30 reports split resident groups
for tool-lint, lint-libs, lint-artifacts/fast, and tail cohorts. Keep open:
tail-pure is 7532ms, lint-libs/core is 7502ms, lint-tools is 7663ms, and
all-strict diagnostics is 7906ms.

2026-07-01 post-warm-launcher removal proof: macos-arm64-12x2 hot full suite
passed at 30016ms internal / 32.23s wall. Current tails: tail-pure 7063ms,
tail-process 6014ms, tool-doc 8204ms, tool-repair 9335ms, lint-libs/core
7490ms, lint-tools 6994ms. Keep open: acceptance is each tail/lint slice under
5s, and tool-doc/tool-repair/lint-libs still exceed that.

2026-07-01 direct-core proof: after the check-tool file-label spawn removal,
macos-arm64-12x2 hot full suite passed at 24878ms internal / 27.22s wall.
Remaining above-5s tails: tool-doc ~8.1s, tool-repair ~9.6s, lint-tools ~6.2s,
lint-libs/core ~5.8s, tail-pure ~5.7s, tail-process ~6.3s. Keep open.

2026-07-01 check-all-errors cleanup proof: `tools/check-all-errors-test.f`
removed its warm-run/process CLI smoke and tests the argv contract in-process.
Full macos-arm64-12x2 persistent-cache suite passed at 24117ms internal /
26.48s wall. Current above-5s tails: tool-doc 8405ms, tool-repair 7200ms,
lint-tools 5749ms, lint-libs/core 5801ms, tail-pure 5684ms, tail-process
5965ms. Tool-repair improved materially but remains above the 5s target.
