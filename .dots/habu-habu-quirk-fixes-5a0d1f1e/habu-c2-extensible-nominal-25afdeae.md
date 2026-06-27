---
title: "C2: extensible nominal sig types"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T13:15:57.199337+02:00"
---

Make signature type-tokens extensible: allow declaring nominal cell-types (deftype node/track) or auto-register an unknown sig token as a fresh distinct cell type. Turns 'sig is types only' from a quirk into a Zig-style distinct-types win (a node can't be passed where a len is wanted). Builds on the existing nominal roles (idx/len/count...). src/core/checker.f.
