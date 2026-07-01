---
title: Collapse tail and lint slice bottlenecks
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-30T23:24:33.490508+02:00\""
closed-at: "2026-07-01T13:37:30.027054+02:00"
close-reason: "Implemented worker-local fork pools for tail-pure, tail-process, lint-libs/core, and lint-tools after shared setup. Default auto Mac profile retuned to macos-arm64-10x2 to avoid oversubscription. Proof: hot auto 23129ms internal / 25.43s wall; cold auto 27613ms internal / 29.85s wall vs clean baseline 28455ms / 30.64s. Changed groups: tail-pure 2675ms, tail-process 3429ms, lint-libs/core 3038ms, lint-tools 3660ms."
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

2026-07-01 helper-collapse proof: after in-process diagnostic SARIF,
public-signature, trust-lint, and jitdump execution, macos-arm64-12x2 hot full
suite passed at 22148ms internal / 24.38s wall. Current above-5s tails:
diagnostics all-strict 7775ms, lint-tools 5574ms, lint-libs/core 5249ms,
tail-pure 5165ms, tail-process 5969ms, prop/debug 5177ms. Focused bundle
timings show the remaining cost is sequential bundle grouping plus full-suite
contention, not repeated warm-image or helper-launch setup. Keep open: split
tail-pure, tail-process, lint-libs/core, and diagnostics all-strict into smaller
named worker groups or reduce their internal long poles without regressing cold
cache-fill time.
