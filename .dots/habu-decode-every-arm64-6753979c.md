---
title: Decode every ARM64 instruction Habu emits
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:41:53.303715+03:00"
---

Problem: tools/jitdump.f (decoder JIT-DIS:STEP, tools/jitdump-arm.f / src/arch/arm64/disasm.f) printed ? for 64 of the 178 instructions in six tier-0/tier-1 listings (2026-10-23, FIB/SUMSQ/MIX in ~/.cache/tmp/jerry-tiers/): bl, b.cond, str/ldr pre- and post-index, ldur/stur, ldp/stp, lsl, madd, and immediate. Decoded operands print as bare numbers, one per line, with no register names or branch targets. llvm-objdump decodes them all. Acceptance: every encoding src/arch/arm64/asm.f can produce decodes to its mnemonic, registers and immediates, one line per instruction with branch targets as offsets; a native-suite row decodes each encoder's output and compares it with what was encoded (the encoder is the oracle, not llvm). Verify: the row; jitdump on the three words matches llvm-objdump. Claim: unassigned.
