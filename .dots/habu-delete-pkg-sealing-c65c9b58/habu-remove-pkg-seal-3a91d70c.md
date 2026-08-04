---
title: Remove package-seal claims
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T22:10:51.289700+02:00"
blocks:
  - habu-delete-checker-pkg-476c96b4
  - habu-delete-owner-pkg-2e5b14dc
  - habu-delete-owner-pkg-196de5cb
---

After every package-seal source leaf lands, update `docs/forth.md`,
`docs/type-system.md`, `docs/type-families.md`, `docs/registry-band.md`,
`docs/stdlib.md`, `docs/effects.md`, and `TYPE-FIXES-PLAN.md` to state that
ordinary packages are reopenable and only generated constructor wordlists
remain protected.

Acceptance: a scoped source census finds no `RESTAB`, `LRESTAB`,
`C-SEAL-PACKAGE-FAIL`, `C-SEAL-MATCH`, `C-QUALIFY-SEAL-GUARD`,
`C-PACKAGE-SEAL-GUARD`, `CHECKER-SEALED-PKG?`, `E-EXPORT-SEALED`,
`EXPORT-SEAL-GUARD`, `SEAL-PACKAGE`, ordinary `get-current prot-wid-add`
site, `test/seal-package.f`, or sealed-system-package prose. Retain only the
protected-WID providers and generated-constructor callers,
`C-PACKAGE-PROT-GUARD`, protected publication/AOT restoration,
`SEAL-CAPTURE`, `SEAL-FRIEND`, `SEAL-VIOLATION`,
`layout-buffer-seal.f`, `lower-cert-seal.f`, and unrelated state-machine
seals. Do not add a lint, broad zero-seal rule, API history, compatibility
note, or unrelated documentation rewrite.
