---
title: Remove fixed source-size ceilings from build makers
status: active
priority: 1
issue-type: task
created-at: "\"2026-08-23T18:41:29.422678+02:00\""
---

Remove PMAX-style source buffers in src/habu/build.f and aot-lib.f using the smallest existing dynamic allocation shape. Prove large sources through tools/hb-build.f; do not raise constants.
