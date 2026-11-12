---
title: Design checking a definition in the one reading
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:41:43.070920+03:00"
---

Target: docs/compilation.md 'Defining a word' (:19): one reader; the checker records checked events; the platform code generator compiles from them.

Problem: the checker still reads a definition apart from the reader that compiles it. The check hook re-reads each definition's source (src/core/check-hook.f:240 CHECK:DEFINITION), and a retry pass reads a body again (src/core/checker.f:25762 CHECK-RETRY and the RESCAN flag at :24384; 18 lines of checker.f name one or the other). The Habu reader on arm64 is not yet the one reader either: src/habu/interpreter.f's capture, check-hook.f's re-check, src/habu/verify-source.f's body parser and src/core/type-family.f:2284 TYPE-NAME:CONTROL?'s list of control words each know the language's control words again. Tier 1's re-read (src/compiler/native/tape.f, feed.f, elaborate.f, hir-word.f) goes with habu-delete-tier-1-f5c44fb4.

Acceptance: a design, decided, for checking each definition during its one reading: what the checker records, how CHECK-RETRY and RESCAN go, and which of the four arm64 control-word lists become readers of it; then lanes are filed as dots.

Depends: none.

Claim: unassigned.
