---
name: habu-gate
description: Use when running Habu focused tests or the full native gate.
---

# Habu Gate

Use native `bin/hb` for all gate work. Select checks using the repository's
[testing policy](../../AGENTS.md#verification-and-documentation) and run focused
tests through their owning load paths.

When changed behavior is baked into the engine, build a private product from
the exact tree with `tools/native-build.f`, then copy the candidate and its
`.map` file into that private tree's `bin/` as `hb` and `hb.map`.

When the impact rules require the full native gate, run it there:

```sh
bin/hb --load test/run.f </dev/null
```

Keep generated artifacts out of the commit and delete gate caches only when
testing uncached behavior. See [the gate instructions](../../docs/gate.md) for
candidate setup and execution.

Run generation convergence and Gforth recovery checks only for the impacts
named in the testing policy; [bootstrap.md](../../docs/bootstrap.md) gives
their commands.
