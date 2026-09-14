---
title: Restore optimizing dispatch in fresh compiler-chain products
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T12:14:20.566878+03:00"
---

Owner cedar. After a24d8aac fixes graph alignment, /tmp/habu-cedar-aligned/tmp/hb-stdin boots and runs JIT definitions, but its test/compiler/native-case.f optimizing-tier cases exit 82: hb: native compiler dispatch unset. Trace capture and fresh boot installation of NCOMP-DISPATCH:XT-CELL. Acceptance: both tiers through the emitted product, then standalone native build/capture/restore/REPL. Do not infer product acceptance from the older bin/hb integration engine.
