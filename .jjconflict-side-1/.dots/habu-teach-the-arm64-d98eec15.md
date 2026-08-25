---
title: Teach the ARM64 disassembler the unscaled forms
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T21:54:19.674049+02:00"
---

src/arch/arm64/disasm.f prints '?' for Ldur and Stur, which are now ordinary instructions in emitted code: since habu-place-the-data-9f128e58 a routine stands its data-stack pointer where the fewest adjustments are needed, so an access one cell UNDER the pointer is the common case and it is encoded in the unscaled signed form. tools/jitdump.f is the first tool anyone reaches for on a codegen crash, and it now renders the two instructions that carry every argument and every result of a compiled word as a question mark. The decode itself is four lines - mask $FFC00000 against $F8400000 and $F8000000, offset is a signed nine-bit field at bit twelve - and was written and then withdrawn from the placement commit for one reason: disasm.f is a wholly global file and tools/package-diff-lint.f reports every definition it changes (E-PACKAGE-OWNERSHIP), while src/arch/arm64/asm.f, icode.f and mnem.f carry exact-path exemptions and disasm.f does not. So the work is: decide whether disasm.f joins that exempted set (it is the same ARM64 encoder prefix family, and it is loaded by tools that name DIS1 and DISASM bare) or becomes a real package with its callers qualified - tools/jitdump-core.f and tools/imagedisasm.f are the callers - and then add the two forms with a test that decodes a known word each way. Owner: src/arch/arm64/disasm.f.
