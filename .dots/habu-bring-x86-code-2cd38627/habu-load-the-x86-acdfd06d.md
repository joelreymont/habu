---
title: Carry each target's full disassembler in its engine
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:47:58.485119+01:00"
---

Problem: Joel 2026-11-26: "make sure ARM64 also carries the full disassembler, and all platforms do in the future!" src/habu/debug.f:11-12 gives x86's hb its full disassembler (src/arch/x86-64/disasm.f) but arm64's only asm.f; arm64's disassembler (src/arch/arm64/disasm.f) loads only with tools/code-dump-arm.f. Acceptance: the arm64 engine's map holds src/arch/arm64/disasm.f's words and x86's holds x86-64/disasm.f's; debug.f says every engine carries its target's full disassembler; the fixpoint and the code-dump tests pass on both. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-x86-dis.
