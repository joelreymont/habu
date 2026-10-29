---
title: Run the full suite through T0
status: open
priority: 1
issue-type: task
created-at: "2026-10-29T20:55:09.899143+02:00"
---

Roadmap step 2 (Joel 2026-10-29: a working tier 0 first, performance after). A switch that makes T0 the compiler of every checked definition in every suite row, then bin/hb --load test/run.f. Every refusal (E-T0-UNSUPPORTED) and wrong answer becomes a small lane with a failing test first; known already: field-loan, init and stow sites (habu-compile-field-loan-5b06237b). Open: the cheapest row mechanism (per-row T0 install from source is too slow; a snapshot with T0 installed, or an engine carrying T0). Proof: full suite green on T0.
