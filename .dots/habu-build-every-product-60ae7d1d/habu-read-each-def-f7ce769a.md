---
title: Design checking a definition in the one reading
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:41:43.070920+03:00"
---

Target: docs/compilation.md 'Defining a word' (:21): one reader; the checker records checked events; the platform code generator compiles from them.

Problem: the checker still reads a definition apart from the reader that compiles it. The check hook re-reads each definition's source (src/core/check-hook.f, CHECK:DEFINITION), and a retry pass reads a body again (src/core/checker.f:27997 CHECK-RETRY and the RESCAN flag at :26367; 20 lines of checker.f name one or the other). The Habu reader on arm64 is not yet the one reader either: src/habu/definers.f's capture, check-hook.f's re-check, src/habu/verify-source.f's body parser and src/core/type-family.f:2324 TYPE-NAME:CONTROL?'s list of control words each know the language's control words again. The earlier design round (milestones A, incremental checking, and D, the Habu reader authoritative on arm64) lived in a scratch directory, not in the tree. Of its other milestones, E is done (bootstrap/ and forth.fs are gone), tier 1's re-read (B: src/compiler/native/tape.f, feed.f, elaborate.f, hir-word.f) goes with habu-delete-tier-1-f5c44fb4, and the JIT's pass-2 replay (C: the EM-P2 words in src/habu/habu2.f) goes with habu-delete-the-jit-d9fdaf67. Arm64 keeps both tiers until then (Joel), and both must read the same events.

Acceptance: a design, decided, for checking each definition during its one reading: what the checker records, how CHECK-RETRY and RESCAN go, and which of the four arm64 control-word lists become readers of it; then lanes are filed as dots.

Files: docs/compilation.md for the design; later src/core/checker.f, src/core/check-hook.f, src/habu/definers.f, src/habu/verify-source.f, src/core/type-family.f.

Verify: the design is in docs/compilation.md and its lanes are dots with acceptance; each lane then keeps the suite and the convergence check (docs/gate.md) green and runs tools/compile-floor.f before and after.

Depends: none.

Ownership: unassigned. Claim: unassigned.
