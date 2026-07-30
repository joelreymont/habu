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

After all package-seal source leaves land, remove stale package-sealing claims from TRUSTED.md and update docs/forth.md, docs/type-system.md, docs/type-families.md, docs/registry-band.md, docs/stdlib.md, docs/effects.md, and TYPE-FIXES-PLAN.md to state that ordinary packages are reopenable and only generated constructor wordlists remain protected. Retire obsolete package-seal work dots without touching the separate protected-WID registry deletion work. Acceptance at M17: scoped absence census for RESTAB, LRESTAB, C-SEAL-PACKAGE-FAIL, C-SEAL-MATCH, C-QUALIFY-SEAL-GUARD, C-PACKAGE-SEAL-GUARD, CHECKER-SEALED-PKG?, E-EXPORT-SEALED, EXPORT-SEAL-GUARD, SEAL-PACKAGE, owner-wid-emit-seal, ordinary get-current prot-wid-add sites, test/seal-package.f, and sealed-system-package prose. Allow only prot-wid-add providers, generated-constructor callers, C-PACKAGE-PROT-GUARD and protected publication/AOT restoration, SEAL-CAPTURE, SEAL-FRIEND, SEAL-VIOLATION, layout-buffer-seal.f, lower-cert-seal.f, and unrelated state-machine seals. No new lint, broad zero-seal rule, API history, compatibility note, or unrelated documentation rewrite.
