---
title: Print one disassembled instruction per line
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T18:39:08.506865+03:00"
---

Problem: the arm64 disassembler (src/arch/arm64/disasm.f) prints each operand with the engine's `.`, which ends its number with a newline (`1 . 2 .` writes "1\n2\n", src/habu/aot-decl.f:781-782). In DLS1 and DLS4 (:36-40) and DROW-EMIT (:49-56) each operand therefore ends its own line, and a row with no operand runs into the next instruction. Its users are tools/imagedisasm.f and tools/jitdump-arm.f. ERROR-TEXT:DIGITS$ (src/core/util.f:116) gives a number's digits with no space or newline.

Acceptance: the disassembler prints one instruction per line, its operands separated by ", ", the line ended after the last operand or after the mnemonic when there is none; a case showing the current output fails first.

Files: src/arch/arm64/disasm.f, tools/imagedisasm-test.f.

Verify: tools/imagedisasm-test.f (suite imagedisasm-tool, test/gate-stdlib-cases.f:78) with the new case, red before the change and green after.

Depends: none.

Ownership: unassigned. Claim: unassigned.
