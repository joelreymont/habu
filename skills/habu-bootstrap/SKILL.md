---
name: habu-bootstrap
description: Use when recovering or refreshing Habu bin/hb.
---

# Habu Bootstrap

Use this skill for `bin/hb` recovery and self-refresh. Run from the
repository root.

Recover a missing or broken `bin/hb` from a trusted native hb of the same OS
and machine (docs/bootstrap.md "Recovery"):

```sh
HABU_SEED_SHA256=<hex> /path/to/trusted-hb --load tools/seed-main.f \
  -- /path/to/trusted-hb
```

Build a private hb from the current tree:

```sh
TMPDIR=$TMP "$HOST" --load tools/hb-build.f -- --engine -o "$OUT"
```

Check that successive builds converge with `tools/chain-run-build.f`
(docs/bootstrap.md "Convergence check"), then qualify the converged hb with
its matching source tree before replacing `bin/hb` (docs/bootstrap.md
"Refresh `bin/hb`").
