---
title: "C2: extensible nominal sig types"
status: closed
priority: 2
issue-type: task
created-at: "2026-06-27T13:15:57.199337+02:00"
closed-at: "2026-06-29T04:58:31.630147+02:00"
close-reason: "Implemented explicit DEFTYPE nominal signature types: dynamic copied type registry, duplicate/reserved-name rejection, candidate/scope rollback of type registry, top-level checked DEFTYPE word, unknown tokens still E-UNKNOWN-SIGNATURE-TYPE. Verified fixpoint rebuild, engine-suite, trust-lint, typed-diff lint, host/filemap lint, full native gate 70830ms <= 90000ms."
---

Make signature type-tokens extensible: allow declaring nominal cell-types (deftype node/track) or auto-register an unknown sig token as a fresh distinct cell type. Turns 'sig is types only' from a quirk into a Zig-style distinct-types win (a node can't be passed where a len is wanted). Builds on the existing nominal roles (idx/len/count...). src/core/checker.f.
