---
title: engine minor defects and dead names
status: closed
priority: 3
issue-type: task
created-at: "2026-08-22T22:47:07.240654+02:00"
closed-at: "2026-09-16T14:34:52.149308+03:00"
close-reason: "superseded by habu-campaign-c7-learning-b18a8259: Residue: the fail-open definer hook, the dead names and the misplaced files are still there; only the PTX test under src/ has left."
---

Problem: habu2.f:2675-2682 C-DEFHOOK ignores the hook verdict (documented fail-open for create/variable/constant rows); dead: habu1.f:477-483 LNX-SKIP/FAIL/CHILD/CLOSEFAIL/WAIT, :994 SAD-HAS, habu2.f:997 and forth.fs:2027 PFX-LOAD-FILES, src/habu/rt.f:751-817 EMIT-DOT/EMIT-ATOI/DOT-LBL/ATOI-LBL, src/os/macos/macho.f:14 PIE? and sign2.f:51 EXECSEG-LIM (never toggled); src/habu/debug.f:111 MAXBP 8 vs forth.fs:178 '16 breakpoints, 16 B each' vs the 32-byte stride; habu1.f:3688-3695 documents clobber-lint modelling LFIND without x5; 'kernel:' accepted as a colon synonym (habu2.f:6115, forth.fs:4679) and used only by maki and lints; src/arch/ptx/vjp-test.f is a test under src/; src/arch/arm64/disasm.f is tool code beside the engine assembler. Acceptance: each deleted, moved or corrected. Files: as listed. Verify: gate. Depends: none. Ownership: engine. Claim: unassigned.
