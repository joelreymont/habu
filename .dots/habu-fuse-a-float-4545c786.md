---
title: Fuse a float comparison into the select that reads it
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T11:27:10.711779+02:00"
---

Problem: src/compiler/native/select.f SEL-FUSE-OF fuses only the six comparisons of two GENERAL registers into a machine select; a float comparison keeps a64.fflag and the select is made on the number it answers, which costs a Cset, a negation and a compare-against-zero where an Fcmp and a Csel would do. The answers are already right - a64.fflag answers zero for a NaN because the conditions it is given are the three that are false under the unordered flag - so this is a cost item and not a correctness one. Acceptance: a64.fcmpsel and a64.fcmpselz in src/compiler/native/a64ir.f taking their compared values from the floating file and answering a general register, SEL-FUSE-OF extended to the freg and fzero kinds, and a case in test/compiler/native-select.f plus a NaN row in tools/codegen-compare-corpus3.f proving the fused float select takes the same arm the interpreted word does. Files: src/compiler/native/a64ir.f, src/compiler/native/emit.f, src/compiler/native/spill.f, src/compiler/native/select.f, test/compiler/native-select.f. Verify: bin/hb --load test/compiler/native-select.f, native-a64ir.f, native-chain.f, tools/codegen-compare.f. Depends: none. Ownership: the a64 dialect and the selector. Claim: unassigned.
