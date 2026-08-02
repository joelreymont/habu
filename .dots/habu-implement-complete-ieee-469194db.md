---
title: Hard-cut global F32 conversion aliases
status: active
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:19.351961+02:00"
---

The bit-exact IEEE-754 implementation and exhaustive class tests now live in
the sole numeric owner, `lib/float32.f` package `F32`, but `lib/ptx/cg.f` still
publishes global forwarding aliases `F64>F32` and `F32>F64`. Hard-cut those two
aliases: migrate every live executable caller and current documentation to
`F32:NARROW` and `F32:WIDEN`, then delete the forwarding definitions. Preserve
the canonical conversion behavior, tests, and PTX numerics byte-for-byte.
Ownership: the two aliases and their complete live caller surface. Acceptance:
zero operational legacy reference remains; float32, PTX headers/codegen,
device-independent numerical goldens, Maki, typed/package exact-diff, and full
native gates pass. Forbidden: alias, shim, compatibility import, removed-name
assertion, tombstone, lint, ledger, versioning, fallback, or unrelated numeric
refactor. Claim: agent=f32_hard_cut workspace=.jj-ws/habu-delete-f32-aliases
