---
title: Canonicalize typed native IR
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T11:52:36.913344+02:00"
blocks:
  - habu-record-native-slice-a01a8ad7
---

Compiler-IR reconciliation: after the Wave 2 vertical slice, own deterministic structural SIR/LIR/A64IR optimization outcomes that are not already the basic Wave 2 fold/DCE passes. Preserve explicit numeric, target, effect, CFG, and witness contracts; no direct byte rewriting or old-emitter peepholes. Acceptance: each retained rewrite has pass-specific mutation and differential fixtures, accepted witnesses, measured attribution, and independent stage verification.

Measured 2026-07-19 addendum: a fresh-fixpoint census found 32 standard framed words
in the 4,296-record startup dictionary. Every one ends in a call immediately before
the restore/return epilogue; none is a call-free leaf frame. After direct `BL` lowering
lands, tail-call canonicalization can restore LR/SP and branch to the callee, removing
4 bytes per site, 128 bytes in this startup image. Pin this census as a structural
regression and require the pass to remove all eligible sites without changing dynamic
calls or exception/unwind behavior.
