---
title: Give the arm64 disassembler a package
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:46:51.116488+02:00"
---

Problem: src/arch/arm64/disasm.f defines its words (U64@, U32@, FRD, FRN, SPC, DTRUE, DIS1, DISASM and the rest) with no package, against AGENTS.md 'Every module has a real package'; its callers are tools/imagedisasm.f:121 (DISASM) and tools/jitdump-arm.f:12 (DIS1). Acceptance: the file opens a package, DIS1 and DISASM are its public words, the two tools call them qualified, tools/imagedisasm-test.f passes. Files: src/arch/arm64/disasm.f, tools/imagedisasm.f, tools/jitdump-arm.f. Verify: bin/hb --load tools/imagedisasm-test.f. Depends: habu-the-arm64-disassembler-876daec0 (same file). Ownership: those files. Claim: unassigned.
