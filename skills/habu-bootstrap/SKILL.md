---
name: habu-bootstrap
description: Use when recovering or refreshing Habu bin/hb, or validating the no-binary bootstrap.
---

# Habu Bootstrap

Use this skill for `bin/hb` recovery and self-refresh.

## Requirements

- Run from the repository root.
- Prefer a trusted native seed from the same OS and architecture when
  available; see docs/bootstrap.md "Recovery from a trusted native seed".
- The no-binary ARM64 route uses Gforth 0.7.9 or newer with locals
  support. Gforth 0.7.3 is too old. A known-good release is
  0.7.9_20260610.
- With no Gforth installed, use the complang dist tarball named in
  docs/bootstrap.md "Requirements". Its prebuilt kernel images allow a cold build.

Verify locals support:

```sh
printf ': x {: a :} a . ; 7 x bye\n' | /path/to/gforth
```

Recover missing `bin/hb`:

```sh
HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth tools/bootstrap.sh
```

Run the periodic no-install recovery check:

```sh
HABU_BOOTSTRAP_CHECK_ONLY=1 HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth tools/bootstrap.sh
```

Build a private native product from the current tree with a trusted native host:

```sh
HABU_UNDER_TEST=$HOST HABU_FIXPOINT_ENGINE=$HOST HB_TMP=$TMP \
  "$HOST" --load tools/native-build.f -- "$OUT"
```

Build again with `$OUT` as `HOST` and compare successive products. Gate the
converged product with its matching source tree before replacing `bin/hb`;
see `docs/gate.md` "Qualifying a candidate". `tools/build-fixpoint-refresh.f -- install` produces the
source-only recovery engine, not the gated native product.

