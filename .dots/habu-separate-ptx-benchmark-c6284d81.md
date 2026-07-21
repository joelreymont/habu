---
title: Separate PTX benchmark libraries from campaigns
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:03:51.640499+02:00"
---

Production benchmark modules still execute selected campaigns at import time, including gemm-bench.f, and operational docs instruct users to edit a bottom-of-file call to choose another run. Split reusable benchmark definition, exactness/reference logic, campaign registry, and command entry into one-concern package-owned files. Loading a library must allocate nothing, open no CUDA context, spawn nothing, time nothing, mutate no evidence store, print nothing, and run no campaign. Define a closed checked campaign registry and explicit CLI selection that binds workload, configuration, target, numeric policy, warmup, repetitions, output schema, and required correctness gate. Unknown or incompatible campaigns reject before device acquisition. Derive documentation and coverage from the registry; source editing is never an interface. Add import-side-effect counters, every campaign selection, unknown/duplicate entries, device-off behavior, and proof each explicit command still runs its complete correctness and timing protocol. Coordinate the existing autotune import-safety dot for shared mechanics without widening its files. Files: PTX benchmark modules, registry, thin entries, focused tests and generated usage docs. Verify off-device imports, selected live GB10 campaign, PTX standard library, coverage/host/filemap/dot lints, and full native gate.
