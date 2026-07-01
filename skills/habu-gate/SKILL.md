---
name: habu-gate
description: Use when running Habu focused tests, the full native gate, timing the gate, or changing gate pool and budget arguments.
---

# Habu Gate

Use native `bin/hb` for all gate work. Do not add env-var knobs; expose policy as
script arguments.

For timing-regression checks, use `skills/habu-host-profiles/SKILL.md`. The
runner auto-detects the host profile; force `--perf-profile NAME` only when
reproducing a specific policy.

Run the full native gate:

```sh
bin/hb --load test/run.f
```

Run with explicit timing policy:

```sh
bin/hb --load test/run.f -- --pool-slots 12 --nested-pool-slots 2 --budget-ms 70000
```

Run the macOS timing profile:

```sh
bin/hb --load test/run.f -- --under bin/hb --timings
```

Reuse an already-built Habu candidate instead of rebuilding it:

```sh
bin/hb --load test/run.f -- --under bin/hb --pool-slots 12 --nested-pool-slots 2 --budget-ms 70000
```

Run focused tests through their owning load path before the full gate. Keep
generated artifacts out of the commit and delete gate caches only when testing
uncached behavior.

`Habu-under-test` is the small rebuilt `hb-stdin` engine, not a snapshot. Warm
snapshots are covered by the warm-image feature tests; they are not native suite
launchers.
