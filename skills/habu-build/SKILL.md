---
name: habu-build
description: Use when building standalone Habu AOT binaries, REPL images, or validating native hb-build behavior.
---

# Habu Build

Use checked Habu build tools, not host scripts.

Build an AOT binary:

```sh
bin/hb --load tools/hb-build.f -- prog.f -o prog
```

Build a REPL image with `hb-build` when a test or tool needs a baked REPL bundle.
Keep generated images and binaries out of commits unless they are explicit
source fixtures.
