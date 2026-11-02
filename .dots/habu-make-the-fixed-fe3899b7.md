---
title: Make the fixed-body kinds one ENUM
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T11:21:13.675215+02:00"
---

Problem: one set of fixed-body kind numbers (none 0, constant value 1, data address 2, defer 3, cast 4) is spelled as untyped constants in four places: src/habu/layout.f:1421-1423 NCOMP-DISPATCH:FIXED-VAL-KIND/FIXED-ADDR-KIND/FIXED-DEFER-KIND; src/compiler/native/dict.f:123-125 NDICT FIXED-NONE/FIXED-VAL/FIXED-ADDR; src/compiler/native/compiler.f:110-111 FIXED-DEFER and FIXED-CAST (4); src/core/checker-owner-extension.f:97-99 FIXED-NONE/VAL/ADDR, re-exported by src/core/checker-owner-abi.f:350-352 RECORD-FIXED-NONE/VAL/ADDR. The words taking a kind take it as n: NCOMP:COMPILE-FIXED (compiler.f:811), T0 COMPILE-FIXED (src/habu/t0.f:1890), DEF-FIXED (src/habu/definers.f:753) and its AS-COMPILE-FIXED cast (definers.f:56). Joel 2026-11-02 (LESSONS.md): 'the fixed-body kinds become one typed enum in a lane of their own'. Acceptance: one ENUM at the lowest owner that all four loaders reach, in checked code (card: ENUM under 0 set-check is E-TDECL-UNCHECKED); every kind constant and every kind-taking or kind-returning word uses its type; the duplicate constants go; behaviour and image bytes otherwise unchanged. Public words added, removed or renamed need Joel's approval through the interview before they are written (card section 1); list each with its callers. Files: those named. Verify: compiler-native-create-does, tier, outer-interpret, cold-naming, then the full arm64 suite, the generation chain and an x86 cross-build. Depends: habu-install-t0-as-6492133d (landed with it). Ownership: the kind constants and kind-taking effects in those files. Claim: unassigned.
