---
title: "S6 L5b2: the arm64 boot and LINKER:WRITE; a stripped MAIN runs"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T22:23:14.557226+02:00"
---

Problem: design-s6-l5.md L5-2, L5-4, L5-5: no arm64 boot or writer exists for a product linked from a build's space, so no stripped program built this way runs (S7). Acceptance: src/arch/arm64/boot.f A64BOOT START, ( label -- ) (straight-line, text0 at its first word; entry args, signal stack, boot data stack x19, DATA map, DATA copy, slide added to each listed cell, run-time cells, frame stacks, crash install, main BL, exit hook, exit 0); ARM64:KERNEL = OPEN, boot, KERNEL; LINKER:WRITE ( ptr u8 n -- ) links from the global MAIN and calls DRV-EMIT-IMAGE with SIGN-ID:PROG$; the main veneer; RBASE-CELL set where habu2.f's TADR, comment (above TADR,) says, and that comment corrected. Test outside any package via LINKER:WRITE, no defer or does>: MAIN `72 emit 105 emit cr` prints Hi, rc 0; an uncaught throw's rc; an xt! at run time. Files: src/arch/arm64/boot.f, src/arch/arm64/link-kernel.f, src/habu/linker.f or link-data.f, src/habu/habu2.f (comment), a new test. Verify: the brief written after L5b1 lands. Depends: habu-s6-l5b1-a-f44dacbe. Ownership: carl. Claim: unassigned.
