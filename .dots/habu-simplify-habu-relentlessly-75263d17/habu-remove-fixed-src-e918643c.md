---
title: Remove fixed source-size ceilings from build makers
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-08-23T18:41:29.422678+02:00\\\"\""
closed-at: "2026-09-16T14:34:50.598109+03:00"
close-reason: "done: PMAX is gone from both named files [rg PMAX src/habu/build.f src/habu/aot-lib.f: no hits]"
---

Remove PMAX-style source buffers in src/habu/build.f and aot-lib.f using the smallest existing dynamic allocation shape. Prove large sources through tools/hb-build.f; do not raise constants.
