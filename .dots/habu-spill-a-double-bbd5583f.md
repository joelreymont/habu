---
title: Spill a double to a frame slot
status: active
priority: 3
issue-type: task
created-at: "2026-08-03T01:23:12.586419+02:00"
---

Claim: agent=fpplace workspace=.jj-ws/habu-keep-floats-in-9f0fe969

src/compiler/native/a64ir.f declares no STR/LDR form for the D file and src/compiler/native/spill.f refuses by name a value of the floating class it would have to put away. Nothing in the float corpus reaches it: a routine's contract hands out the whole floating register file, so a body would need more doubles live at once than the file holds. What is needed when one does: a64.fstr and a64.fldr beside a64.astore/a64.aload with the D encodings, the spill rewrite mapping the floating class to them, and a regression body that really runs out of D registers. Owners: A64IR, A64SPILL, A64EMIT.
