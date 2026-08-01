---
title: Let a routine of one block call itself
status: open
priority: 4
issue-type: task
created-at: "2026-08-01T15:34:28.647845+02:00"
---

src/compiler/native/regalloc-verify.f VLINK-CK refuses a calling routine whose entry block is also the block control leaves through (rb 0 = throws E-A64RAV-CALL), because the link save's window and the link restore's window would be the same block and the shape rule has no rule for the two overlapping. The only Habu word with that shape is one that calls itself unconditionally - a program that cannot terminate - so the refusal costs nothing today, but it is a shape rule that gives up rather than a statement about the module. Generalise VLINK-CK to a single window when the entry block and the return block are one.
