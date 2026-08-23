---
name: habu-gate
description: Use when running Habu focused tests or the full native gate.
---

# Habu Gate

Use native `bin/hb` for all gate work.

Run the full native gate:

```sh
bin/hb --load test/run.f
```

Reuse an already-built Habu candidate instead of rebuilding it:

```sh
bin/hb --load test/run.f -- --under bin/hb --timings
```

Run focused tests through their owning load path before the full gate. Keep
generated artifacts out of the commit and delete gate caches only when testing
uncached behavior.

`Habu-under-test` is the small rebuilt `hb-stdin` engine, not a snapshot
launcher. Snapshot coverage belongs to the native build/fixpoint path.
