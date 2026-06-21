# Native Seed

`bin/hb` is generated and ignored. A checkout without `bin/hb` is recovered from a
trusted native seed, not from gforth. The checked recovery command is run by the
trusted seed binary itself:

```sh
/path/to/hb-seed --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f src/core/sha256.f \
  lib/codesign.f tools/seed.f tools/seed-main.f -- /path/to/hb-seed

HABU_SEED_SHA256=<hex> /path/to/hb-seed --load lib/errors.f lib/string.f \
  lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f \
  lib/process-env.f src/core/sha256.f lib/codesign.f tools/seed.f \
  tools/seed-main.f -- /path/to/hb-seed
```

The seed is the trust root. Use a previous release asset or a seed copied from a
trusted checkout. `tools/seed.f` validates optional `HABU_SEED_SHA256`, installs
the seed to `bin/hb`, makes it executable, ensures ad-hoc code signing, runs a
stdin smoke program, then runs `tools/build-fixpoint-main.f -- install`. The
installed binary is therefore rebuilt from the current source and must pass the
normal self-rebuild fixpoint.
