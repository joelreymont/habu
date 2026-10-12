# Trusted Native Seed

Use a trusted native hb from the same OS and architecture when one is
available. For ARM64 macOS and Linux with no native binary, the Gforth
recovery route is in [bootstrap.md](bootstrap.md).

A trusted native hb is a recovery seed, not a qualified product. The seed
binary runs the checked recovery command:

```sh
/path/to/hb-seed --load tools/seed-main.f -- /path/to/hb-seed

HABU_SEED_SHA256=<hex> /path/to/hb-seed --load tools/seed-main.f \
  -- /path/to/hb-seed
```

The seed binary's own `--load` path selects its host core/checker/env source
prefix.

Use a previous release asset or a seed copied from a trusted checkout.
`tools/seed.f` validates optional `HABU_SEED_SHA256`, installs the seed to
`bin/hb`, makes it executable, ensures ad-hoc code signing, runs a stdin smoke
program, then runs `tools/build-fixpoint-main.f -- install --force`. The
`--force` is deliberate: this is a proof flow, so the content-key stamp skip
must not stand in for an actual rebuild. The installed binary is therefore
refreshed from current source and must pass the normal self-rebuild fixpoint.

After `bin/hb` exists, daily rebuilds do not need the seed:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/memory.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install --force
```
