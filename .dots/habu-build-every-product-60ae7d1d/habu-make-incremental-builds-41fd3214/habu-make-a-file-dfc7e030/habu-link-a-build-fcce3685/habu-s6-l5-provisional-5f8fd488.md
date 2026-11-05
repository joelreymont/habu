---
title: S6 L5 provisional public names (Joel away)
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T21:31:52.039244+02:00"
---

Problem: Joel said 'do not ask me' until he is back; S6 L5 needs public names he would normally pick. Design: .jj-ws.noindex/carl-dots/.out/carl-file/design-s6-l5.md (carl, read at master 16ea4817). Provisional calls, each one question for Joel on return: (1) ENGINE-EMIT:OPEN ( ptr u8 n -- ) and ENGINE-EMIT:KERNEL ( -- ) replace public EMIT-FORTH-IMAGE (src/habu/habu2.f:14061) so a boot can sit at code offset 0 before the kernel emission; path tools/native-emit-arm.f:115 ENGINE-EMIT:FORTH -> OPEN, KERNEL; option instead: boot after the band and an entry input in macho.f:239 and elf.f:107. (2) LINKER:WRITE ( ptr u8 n -- ): WITH's body calls it to link the open space's program from the global MAIN and write it to the path; reached only by the S7 test until hb-build's stripped path moves onto LINKER:WITH (tools/hb-build-core.f:18); option instead: a path argument on WITH. (3) A64BOOT START, ( label -- ) follows X64BOOT START, (package and file are Joel's, DECISIONS 'where machine-specific code lives'). Acceptance: Joel's answer on each; a rename or reversal becomes a fold-in on the landed lane. Files: src/habu/habu2.f, src/arch/arm64/link-kernel.f, src/habu/linker.f, src/arch/arm64/boot.f. Verify: DECISIONS.md entries once Joel settles them. Depends: none. Ownership: carl. Claim: agent=carl.
