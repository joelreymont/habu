---
name: habu-build
description: Use when building standalone Habu AOT binaries, REPL images, or validating native hb-build behavior.
---

# Habu Build

Use checked Habu build tools, not host scripts.

Build an AOT binary:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/source.f lib/build.f \
  lib/codesign.f lib/content-key.f tools/build-fixpoint.f tools/warm-run.f \
  tools/hb-build-lib.f tools/hb-build.f -- prog.f -o prog
```

Build a REPL image with `hb-build` when a test or tool needs a baked REPL bundle.
Keep warm images and generated binaries out of commits unless they are explicit
source fixtures.
