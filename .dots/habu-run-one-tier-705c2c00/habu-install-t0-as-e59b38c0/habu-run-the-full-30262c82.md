---
title: Run the full suite through T0
status: active
priority: 1
issue-type: task
created-at: "2026-10-29T20:55:09.899143+02:00"
---

Roadmap step 2 (Joel 2026-10-29: a working tier 0 first, performance after). A switch that makes T0 the compiler of every checked definition in every suite row, then bin/hb --load test/run.f. Every refusal (E-T0-UNSUPPORTED) and wrong answer becomes a small lane with a failing test first; known already: field-loan, init and stow sites (habu-compile-field-loan-5b06237b). Open: the cheapest row mechanism (per-row T0 install from source is too slow; a snapshot with T0 installed, or an engine carrying T0). Proof: full suite green on T0.

Mechanism chosen (brief /tmp/jerry-big/suite-t0-run-brief.md): a gate setting that inserts a file after --load in every spawned row's argv, and test/run-t0.f setting it to test/t0-child.f (T0:INSTALL, about 0.12 s CPU per row); image-build rows and child processes are not covered until step 3. One run; reds sorted into tier-0 by choice, tier-1 facts, builds inside the row (E-T0-SHADOW), T0 gaps (the product: one small dot each after review), other. Launched 2026-10-30 00:30 on g3. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s2.

2026-10-30 01:25: bin/hb --load test/run-t0.f runs in jerry-s2 since 00:26 (pool pid 92337; load average 150-200 from the prune workers beside it); a waiter reports its end, then a worker sorts the reds.
