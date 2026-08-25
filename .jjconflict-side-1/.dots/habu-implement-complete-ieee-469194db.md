---
title: Hard-cut global F32 conversion aliases
status: closed
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:19.351961+02:00"
closed-at: "2026-08-02T22:06:59.257800+02:00"
close-reason: "Landed on origin/master at 6df493c76fcd: global F32 conversions and buffer helpers hard-cut to F32/F32-BUF/PTX-ACT; full test/run.f, Maki, device, PTX/lint-libs, and exact correction gates green; independent review ACCEPT."
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

Checkpoint-discovered ownership migration: move the byte-identical `SF-ST`,
`SF-LD`, `F32-PACK`, and `F32-UNPACK` implementation out of the legacy-global
PTX module into `lib/float32-buffer.f`, package `F32-BUF`, with the exact public
surface `STORE`, `LOAD`, `PACK`, and `UNPACK`; migrate its live callers and
delete all four globals. Package `lib/ptx/cg-activation.f` under its real owner
`PTX-ACT`, publish only the emitters proven by its live external-caller census,
and qualify those callers. These are the complete owner migrations exposed by
the exact package gate; no generic compatibility owner or wider CG packaging is
in scope.
