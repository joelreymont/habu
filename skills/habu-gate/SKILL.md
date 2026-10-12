---
name: habu-gate
description: Use when running Habu focused tests or the full native gate.
---

# Habu Gate

Use native `bin/hb` for all gate work.

Build a private product from the exact tree with `tools/native-build.f`, then
copy the candidate and its `.names` file into that private tree's `bin/` as
`hb` and `hb.names`. Run the full native gate there:

```sh
bin/hb --load test/run.f
```

Run focused tests through their owning load path before the full gate. Keep
generated artifacts out of the commit and delete gate caches only when testing
uncached behavior.

Snapshot coverage belongs to the native build/fixpoint path.
