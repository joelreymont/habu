# Trusted Native Seed

`docs/bootstrap.md` is the canonical no-binary recovery procedure: Gforth
creates only private `HB_TMP` artifacts, then the new `bin/hb` refreshes itself
from current source.

A trusted native `hb` seed is still a valid alternate trust root when one is
available. The checked recovery command is run by the seed binary itself:

```sh
/path/to/hb-seed --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f src/core/sha256.f \
  lib/codesign.f tools/seed.f tools/seed-main.f -- /path/to/hb-seed

HABU_SEED_SHA256=<hex> /path/to/hb-seed --load lib/errors.f lib/string.f \
  lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f \
  lib/process-env.f src/core/sha256.f lib/codesign.f tools/seed.f \
  tools/seed-main.f -- /path/to/hb-seed
```

Use a previous release asset or a seed copied from a trusted checkout.
`tools/seed.f` validates optional `HABU_SEED_SHA256`, installs the seed to
`bin/hb`, makes it executable, ensures ad-hoc code signing, runs a stdin smoke
program, then runs `tools/build-fixpoint-main.f -- install`. The installed
binary is therefore refreshed from current source and must pass the normal
self-rebuild fixpoint.

After `bin/hb` exists, daily rebuilds do not need the seed:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/build.f \
  lib/codesign.f tools/build-fixpoint.f tools/build-fixpoint-main.f -- install
```
