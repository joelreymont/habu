---
title: Resolve recovery full namespace paths
status: open
priority: 1
issue-type: task
created-at: "2026-07-31T06:35:01.144786+02:00"
---

Source dependency: exact reviewed recovery namespace-row candidate plus the frozen native last-separator contract; exact code ancestry enforces ordering inside the stop-the-world branch. Owner: Gforth recovery engine under the existing ENGINE-EMIT vocabulary. Mirror the native E2a single last-separator scanner, malformed-path rejection, exact full-prefix lookup, one full-prefix ensure walker, package-or-type lookup, package-only qualified definition, and missing package-prefix creation. Reuse the recovery E1 LNSFIND and row creator. No second semantics, parent link, side table, compatibility, version, ancestor lookup, using change, nested package blocks, or native edit. Write set: bootstrap/cg/forth.fs and tools/bootstrap-codegen-test.f only. Pre-M17 proof is structural source review only. M17 acceptance runs the real recovered compiler and proves byte/behavior parity for deep lookup, definition, malformed rejection, and type-kind walls.
