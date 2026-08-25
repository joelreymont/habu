---
title: Retire a context abandoned by a throw
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T09:22:31.218244+02:00"
---

Full context: from agent irpass 2026-07-30 while building test/compiler/ir-pass.f. src/compiler/ir/context.f CTX-ENTER runs the caller's quotation and only afterwards clears the registry generation and restores DEPTH (the two lines '0 at GEN!' and 'at DEPTH !'). A throw out of the quotation skips both, so the abandoned context keeps answering IR-CTX:SERIAL-LIVE? true for the rest of the process even though MEM:WITH-BYTES has already unmapped its memory. Every registry that sweeps by owner liveness - IR-ARENA (64 slots), IR-CANON (8 tables), IR-BUILD, IR-PASS - therefore never reclaims that context's slots. Measured: a test whose case builds the two-module fixture (34 arenas) inside a context and lets one refusal escape exhausts the 64-slot arena registry on the very next case, which then fails with E-IR-ARENA-SLOTS (-6657) instead of the refusal it was measuring. test/compiler/ir-pass.f works around it by catching every refusal inside its context; test/compiler/ir-encode.f survives only because its cases leak fewer arenas. Fix: retire the slot and restore DEPTH on the throwing path too, so an abandoned context is indistinguishable from a closed one, and add a regression that opens a context, throws out of it, and requires the arena registry to be reclaimed. Do not weaken the tests to fit the leak.
