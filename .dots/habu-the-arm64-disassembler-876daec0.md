---
title: The arm64 disassembler prints ? for five common instruction forms
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:31:04.524065+02:00"
---

Problem: src/arch/arm64/disasm.f (82 lines; DIS1 :64, DROW-EMIT :49) decodes only a few forms; tools/imagedisasm.f over an engine's BEXEC printed ? for a pre-index LDR, CMP immediate, B.cond, AND immediate and ADR (kernel lane worker, 2026-11-11, output ~/.cache/tmp/quot-kernel/imagedisasm-head.txt and exec-disasm.txt; not rechecked by the lead). Agents debug native failures with this tool (docs/debugging.md), tools/jitdump-arm.f uses the same decoder. Acceptance: those five forms print their mnemonic and operands as the forms already decoded do; cases beside the existing ones in tools/imagedisasm-test.f (suite row imagedisasm-tool, test/gate-stdlib-cases.f:78). Files: src/arch/arm64/disasm.f, tools/imagedisasm-test.f. Verify: bin/hb --load tools/imagedisasm-test.f. Depends: none. Ownership: those files. Claim: unassigned.
