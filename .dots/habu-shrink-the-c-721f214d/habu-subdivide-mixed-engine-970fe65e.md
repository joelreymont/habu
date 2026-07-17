---
title: Subdivide mixed engine size regions
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T23:24:18.133591+02:00"
---

Current map leaves 15464 bytes of post-AOT interpreter/compiler growth hidden outside explicit compile/adt and compile/p2wide buckets. Add zero-target-byte ASM-LEN marks around definition kinds, publication legs, keyword tiers, pass-2 helpers, dictionary helpers, and runtime helper families so every large mixed bucket is attributable. Preserve emitted bytes and byte-identical fixpoint. Files: src/habu/engine-size.f, src/habu/habu1.f, src/habu/habu2.f, test/engine-size-test.f, docs/size-rca.md. Acceptance: before/after bin/hb hashes identical, region sums exact, every residual bucket <=4096 or explicitly decomposed.
