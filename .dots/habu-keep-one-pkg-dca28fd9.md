---
title: Keep one package seal guard
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.808564+02:00"
---

Problem: the package seal guard exists twice: in Habu, src/habu/packages.f PKG-SEAL-GUARD (PKG-SEALED?: CHECKER-SEALED-PKG? or a protected public wordlist), and in hand-written machine code, src/habu/habu2.f:9565 C-PACKAGE-SEAL-GUARD (C-SEAL-MATCH, C-PACKAGE-PROT-GUARD). Unknown which runs for `package NAME`.
Ruling (Joel, 2026-10-08): investigate and keep one implementation, best for the long term.
Acceptance: measured which guard runs on each path (interpret loop, evaluate, tiers); exactly one implementation remains; reopening a sealed package still exits 84 on the product.
Files: src/habu/packages.f, src/habu/habu2.f, src/habu/interpret.f. Verify: test/baked-owner.f and the full suite on the built hb.
Depends: none. Ownership: unassigned. Claim: unassigned.
