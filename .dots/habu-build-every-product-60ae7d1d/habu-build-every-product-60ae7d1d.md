---
title: Build every product as a cross-build with one compiler
status: open
priority: 1
issue-type: task
created-at: "2026-11-04T11:48:45.259779+02:00"
blocks:
  - habu-make-incremental-builds-41fd3214
  - habu-take-checker-working-6703bc4f
  - habu-make-cross-target-934166cc
  - habu-move-the-interpreter-d35780fa
  - habu-cross-build-a-cb630896
  - habu-name-the-build-abed309a
  - habu-run-one-tier-705c2c00
  - habu-share-one-front-199b02a2
  - habu-read-each-def-f7ce769a
---

Why: Joel, 2026-11-04 (DECISIONS "how Habu builds Habu", "one compiler"): an arm64 build is a cross-build whose product is data the build writes, and every machine runs one typed interpreter; the front end is shared and only code generation differs per architecture. Pending work that patched the machinery these decisions replace was closed by the 2026-11-04 census (712 dots read); what remains of the build and compiler work sits under this dot.

Order (each step lands as small green commits; a later step starts when the one before it lands):
1. The cross-build and the one linker: design /tmp/jerry-big/design-xbuild.md on habu-make-a-file-dfc7e030 (it folds the arm64 interpreter switch), under habu-make-incremental-builds-41fd3214. Prerequisites: checker working state outside DATA (habu-take-checker-working-6703bc4f); symbolic target data for create, here and allot (habu-make-cross-target-934166cc); pointer cells known by declared type (habu-save-images-by-f0841d0b, habu-mark-typed-storage-1cc20b92).
2. arm64 end to end: the arm64 product takes the x86-64 product's shape, one typed interpreter (habu-move-the-interpreter-d35780fa, habu-boot-the-arm64-d0d4421a); hb is linked and skips the startup reload (habu-load-hb-build-00d1dd6a); a Linux arm64 product cross-built on macOS (habu-cross-build-a-cb630896); the build names (habu-name-the-build-abed309a).
3. x86-64 through the same linker (habu-build-x86-64-3ed6f45d).
4. Wasm on T0 and the one linker (habu-emit-wasm-from-0d5dab47).
5. Deletions: tier 1 (habu-delete-tier-1-f5c44fb4), the JIT (habu-delete-the-jit-d9fdaf67), the arm64 seed paths (habu-delete-the-arm64-b319376e), held by DECISIONS "the T0 release" until T0 is as fast.
Beside steps 1-4: one T0 front end (habu-share-one-front-199b02a2, habu-copy-every-tier-86155a25, habu-make-a-cb-6de47af1, habu-read-each-def-f7ce769a, habu-compile-field-loan-5b06237b) under habu-run-one-tier-705c2c00.

Other replacements, not ordered against this one: TRUSTED: and privacy (habu-shrink-trusted-to-f4e84345, habu-delete-the-trusted-42b30edd, habu-move-engine-internal-fc2ba551, habu-delete-the-internal-3a818b8a); the hard cutover (habu-drop-schema-and-12ddb887, habu-retire-the-engine-b5850c5c, habu-delete-the-unused-0f33a3e4); hooks to plain calls (habu-remove-the-checker-4425bcef, habu-make-the-src-f0bdcbe1, habu-move-type-family-c1875868, habu-simplify-the-lower-2f175f5c); fork only to exec (habu-fork-only-to-59d3557e).

Rule (Habu CLAUDE.md "Machinery being replaced"): no change extends or patches replaced machinery; a defect it shows becomes an acceptance case of the replacement here.
Campaign only; do not dispatch.
Claim: unassigned.
