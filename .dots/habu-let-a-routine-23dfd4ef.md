---
title: Let a routine contract name a floating argument or result place
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T11:17:28.327913+02:00"
---

Problem: A64EFF's placeseq carries a register NUMBER and no file, and src/compiler/native/regalloc.f FIXED-POOL-CK holds every declared argument and result place against the GENERAL pool alone (F-GPR ... POOL-HAS?). So a contract cannot say 'this argument arrives in d0' at all: a routine that takes or returns a double in a register is unrepresentable, and the closed-world reading is invisible because the only thing a place can be is a general register.

Today nothing needs it - a Habu word passes its arguments on the data stack, and the floating values inside a routine are produced by its own operations - so this is a gap and not a defect. It becomes a defect the moment a routine has to interoperate with a convention that passes doubles in registers (tools/codegen-compare-cabi.f is the consumer that would ask).

Acceptance: decide whether the file belongs on the place (A64EFF:SEQ-WITH gains a file, and REG-POSITIONS/FIXED-POOL-CK/FIXED!/MB-FIX!/MB-WANT! read it) or whether a second placeseq per file is the honest shape; then FIXED-POOL-CK holds each place against ITS file's pool. Regression: a contract naming a floating result, allocated and accepted, with the value leaving in that register; and a contract naming a register no file's pool holds, refused.

Files: src/compiler/a64-effect.f, src/compiler/native/regalloc.f, src/compiler/native/regalloc-verify.f. Verify: bin/hb --load test/compiler/native-regalloc.f; test/compiler/a64-effect.f; test/run.f. Depends: none. Ownership: those three files. Claim: unassigned.

Found while fixing habu-key-a64rav-interference-151111d3: the two-file fixture there had to declare its interface as SEQ-NONE because a floating place cannot be written.
