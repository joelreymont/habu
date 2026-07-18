---
name: habu-bootstrap
description: Use when recovering or refreshing Habu bin/hb, selecting Gforth, validating no-binary bootstrap, or porting bootstrap work to zed/Linux.
---

# Habu Bootstrap

Use this skill for `bin/hb` recovery and self-refresh.

## Requirements

- Run from the repo root.
- On a fresh machine, FIRST prefer a `bin/hb` seed copied from a trusted
  checkout of the same OS/arch (docs/seed.md) over any gforth bootstrap.
- Gforth is only the no-binary recovery host.
- The recovery host must be Gforth 0.7.9 or newer with `{:` locals support.
- Known-good host: `gforth 0.7.9_20260610`.
- **Gforth 0.7.3 is BANNED for Habu work — never install or build it, not
  even as a stepping-stone build host.** With no gforth present at all, use
  the complang *dist* tarball of the pinned snapshot (ships pregenerated
  kernel images, builds with no pre-existing gforth):
  `https://www.complang.tuwien.ac.at/forth/gforth/Snapshots/0.7.9_20260610/gforth-0.7.9_20260610.tar.xz`
  — the GitHub tag archive does NOT self-build (PREFORTH needs a gforth).
- Prefer `~/.local/bin/gforth` when present.

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

Refresh the self-hosted engine:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install
```

If the package manager lacks Gforth 0.7.9+, build a current snapshot and install
or wrap it under `~/.local/bin/gforth`; keep the exact snapshot command in
`docs/bootstrap.md`.

On zed, validate local macOS bootstrap first, then port the same committed tree
and use zed only for Linux/aarch64 proof.
