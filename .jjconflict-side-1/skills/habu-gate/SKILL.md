---
name: habu-gate
description: Use when running Habu focused tests or the full native gate.
---

# Habu Gate

Use native `bin/hb` for all gate work.

Build and install the exact tree, then run the full native gate:

```sh
bin/hb --load tools/build-fixpoint-refresh.f -- install --force
bin/hb --load test/run.f
```

Run focused tests through their owning load path before the full gate. Keep
generated artifacts out of the commit and delete gate caches only when testing
uncached behavior.

Snapshot coverage belongs to the native build/fixpoint path.
