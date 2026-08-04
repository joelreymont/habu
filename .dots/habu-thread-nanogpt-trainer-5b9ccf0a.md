---
title: Thread NanoGPT trainer configuration explicitly
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:07:52.725513+02:00"
---

maki/examples/nanogpt/adam-train.f reads training configuration at load time and from ambient mutable values, while tests reset leaked state between runs. Define an immutable package-owned trainer-config value containing every hyperparameter, model/data identity, schedule, seed, step bounds, and output policy, validate it once, and pass it explicitly into a trainer instance with owned optimizer/model/workspace state. A run must not consult load-time globals, environment state, or prior tests after construction. Failed construction or step leaves the prior instance valid; two trainers with different configs can interleave deterministically. Reuse the general Adam state owner for optimizer mechanics without coupling config identity to ambient cells. Add two-config interleaving, repeated load/run, nested trainer, failure/retry, seed/config mutation, zero/edge steps, exact loss/update snapshots, and proof module import has no side effects. Remove reset helpers whose only purpose is singleton cleanup. Files: NanoGPT Adam trainer and focused tests/callers. Verify NanoGPT training suites, Maki, typed-local/package/host/dot lints, and full native gate.
