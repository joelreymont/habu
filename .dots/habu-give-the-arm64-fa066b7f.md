---
title: Give the arm64 disassembler a package
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T08:46:51.116488+02:00"
---

Problem: src/arch/arm64/disasm.f defines its words (U64@, U32@, FRD, FRN, SPC, DTRUE, DIS1, DISASM and the rest) with no package, against AGENTS.md 'Every module has a real package'; its callers are tools/imagedisasm.f:123 (DISASM) and tools/code-dump-arm.f:12 (DIS1). DISASM ( str -- ) (:228) reads its str's length as an instruction count, so tools/imagedisasm.f:124 builds a str of IMGD-N instructions: a str's length counts bytes. Acceptance: the file opens a package, DIS1 and DISASM are its public words, DISASM takes the code's bytes as a str and decodes its length / 4 instructions, the two tools call them qualified, tools/imagedisasm-test.f passes. Files: src/arch/arm64/disasm.f, tools/imagedisasm.f, tools/code-dump-arm.f. Verify: bin/hb --load tools/imagedisasm-test.f. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-a64dis.
