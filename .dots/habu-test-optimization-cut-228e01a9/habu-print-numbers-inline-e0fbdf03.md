---
title: Print numbers inline through one engine printer
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T18:39:08.506865+03:00"
---

Lane 494 (srcinline, dot 70a8af22, change osssmrwk): the engine's . ends its number with a newline (1 . 2 . writes 1\n2\n) and FMT:.INT is not engine-provided (lib/fmt.f adds 56 records and cannot load before a capture window), so src/ carries its own digit loops: outer.f, repl.f, driver-io.f, render.f, aot-closure.f, and AOT-BUF:.INT in aot-decl.f (added by 494 for the AOT layer). src/arch/arm64/disasm.f DLS1, DLS4 and DROW-EMIT print operands with ., so each operand ends its own line and an operand-less row runs into the next instruction. Acceptance: a census of every src/ digit loop and mid-line . (sink: fd or buffer; signed/unsigned; base); one engine-provided conversion of a cell to decimal digits (signed, MIN-N exact, no space, no newline) loaded before all of them, which every site uses with its own sink; the private loops deleted; the disassembler prints one instruction per line, operands separated by ', ', the line ended after the last operand or the mnemonic; a failing case first for the disassembler and for each changed diagnostic reachable from source; rebuild, g1 == g2, two-gen.
