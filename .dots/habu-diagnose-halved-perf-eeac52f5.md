---
title: Diagnose halved perf budget under concurrent load
status: open
priority: 3
issue-type: task
created-at: "2026-07-20T14:50:48.682313+02:00"
---

Three times on 2026-07-20 the merge battery's test/run.f reported a halved budget (b=21090..21470 vs the normal 38850..40250 for the same tree and calibration 106-110ms) and hard-failed the perf verdict while correctness stayed green; an immediate standalone rerun on the identical tree passed in-band with the normal budget each time. The perf-profile banner line was absent from the failing runs' captured output. Hypothesis space: the macos-arm64-10x2 profile selection or wall-budget derivation degrades when worker lanes load the machine (calibration-vs-budget race, cache-root contention in the persistent profile rows, or a fallback budget path taken silently). Find where the budget b is derived (test/run-lib.f perf machinery), make the selected budget path and its inputs part of the printed verdict line so a degraded selection is visible not silent, and either fix the derivation under load or make the inadmissibility explicit (the failing runs printed admissible=t, so the gate believed the measurement was clean - that is the bug: a halved budget with clean calibration should not be possible). Reproducers: battery runs bxq3by9o8 (b=21090), b62quiwcc (b=21470), bn1nuo7ym (b=21280) in the 2026-07-20 orchestrator session logs.
