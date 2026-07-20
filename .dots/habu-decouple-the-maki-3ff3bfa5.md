---
title: Decouple the maki library from the nanoGPT example
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T13:04:58.885401+02:00"
---

Flagged by the restructure landing (cec8db65): four pre-existing library->app dependencies now cross the maki/ -> maki/examples/nanogpt/ boundary in the WRONG direction. (1) maki/maki.f - the library aggregator - requires examples/nanogpt/from-scratch-train.f, so loading the framework pulls an example; (2) maki/eval/train.f (the eval train leg) requires the example's model+train+adam-train; (3) maki/checkpoint-test.f (library activation-checkpoint test) uses the example model as its test vehicle; (4) maki/adamw-test.f and maki/pos-embed-test.f reach into example trainer helpers (policy/init words, gradcheck helpers). Fix direction: the library must not require examples - invert or extract. For (1)/(2): either the aggregator/eval leg drop the trainer requires (consumers that want the example load it explicitly) or the genuinely-generic parts (a minimal train-step interface) extract into the library and the example implements it. For (3)/(4): give the library tests library-owned fixtures (a tiny in-library model vehicle) instead of the example. Each sub-fix is behavior-neutral: suites green before/after, no lock movement. Coordinate with habu-centralize-maki-suite-85c0ab18 (suite-slice architecture) - this dot owns the dependency DIRECTION, that one the slice layout.
