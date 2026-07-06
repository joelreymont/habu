---
title: Typed defining words + provenance mints
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.866786+02:00"
---

Two S/M capabilities retiring ~48 TRUSTED: sites: (1) role-typed variable/constant/create family generalizing PTR-VARIABLE (~24 sites incl. treeshake x17, MLEN, STB-CELL@); (2) named checked provenance mints - MMAP>PTR, VA>PTR, N>CODE-PTR, null-ptr role (~24 sites: DATA-VA x2, LINUX-VA>PTR, INCLUDE-MMAP-PTR, MBUF/CODE/ICODE-TABS, BP-NULL/NULL$, imgdump/imagedisasm, lib P>N/N>P, task x3). Each mint is one audited word with a test; consumers become fully checked. Effort M (~3d). checker.f + roles.f + docs.

## Adopted rows (2026-07-06 ledger audit)

This dot is now owner-of-record for two generated-definition `evaluate`
boundaries whose original owner dots were archived with the rows still live
(trusted-inventory strict was red on missing owners):

- TRUSTED.md `src/core/roles.f:DTC-EVAL prim-axiom` (from archived
  habu-declarable-nominal-int-3b0721cc): DTC-EVAL evaluates the auto-derived
  deftype converter pair built as TRUSTED: source text (roles.f:32-41). A typed
  defining-word capability mints the converters directly and retires the
  evaluate boundary.
- TRUSTED.md `lib/ffi.f:FDEF-EVAL stdlib-boundary` (from archived
  habu-role-typed-ffi-08f99d18): FDEF-EVAL evaluates generated FFI binding
  definitions (ffi.f:244). Same capability class: a checked defining word for
  FFI: bindings retires it.
