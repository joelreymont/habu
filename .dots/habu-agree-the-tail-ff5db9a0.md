---
title: "Agree the tail routine's stand between the validator and X64SEL"
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T15:18:57.022631+03:00"
---

Problem: src/compiler/native/regalloc-verify.f VDTAIL-CK line 1987 refuses (E-A64RAV-DSTACK) unless VD-STAND equals the RESULT bytes (r * BND-SLOTW) for a routine that leaves through a callee, while select-x64.f stands the body's pointer by the ARGUMENT bytes (D-POS at lines 1344-1347, the entry-base policy the dstand vocabulary states since d2786d19); VDPLACE-CK was taught the vocabulary's stand, VDTAIL-CK was not, so an x86-64 X64ABI:LEAF routine ending in a tail call is refused for a stand the vocabulary declares correct. Acceptance: VDTAIL-CK consults BND-STAND the way VDPLACE-CK does - survey keeps today's check, entry-base checks the base the entry transfer leaves - with a measured tail case under X64ABI:LEAF in test/compiler/x64-regalloc.f (accepted, or refused for a reason that is not the stand), and the ARM64 survey behavior unchanged (test/compiler/native-regalloc.f). Files: src/compiler/native/regalloc-verify.f, test/compiler/x64-regalloc.f. Verify: bin/hb --load test/compiler/x64-regalloc.f; bin/hb --load test/compiler/native-regalloc.f; bin/hb --load test/compiler/native-chain.f. Depends: none. Ownership: allocator validator (hazel). Claim: unassigned.
