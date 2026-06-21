# Native Seed

`bin/hb` is generated and ignored. A checkout without `bin/hb` is recovered from a
trusted native seed, not from gforth:

```sh
tools/seed.sh /path/to/hb-seed
HABU_SEED_SHA256=<hex> tools/seed.sh /path/to/hb-seed
```

The seed is the trust root. Use a previous release asset or a seed copied from a
trusted checkout. `tools/seed.sh` copies it to `bin/hb`, optionally verifies the
SHA-256, runs a smoke program, then runs `tools/build-fixpoint-main.f -- install`. The installed binary is
therefore rebuilt from the current source and must pass the normal self-rebuild
fixpoint.
