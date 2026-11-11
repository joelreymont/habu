---
title: The arm64 disassembler ignores the shift fields of move-wide and add/sub immediate
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:59:02.516105+02:00"
---

Problem: found by the disasm-forms worker (lane commit a5657321, dot habu-the-arm64-disassembler-876daec0), not yet reproduced by the lead: the disassembler prints movz/movk/movn without the hw shift (bits 22:21), so 'movz x0, #1, lsl #16' reads as 'movz x0, #1'; add/sub/cmp immediate ignore the shift bit 22 (imm12 shifted left 12); and the comment 'No locals inside the loop (they corrupt the frame)' states a cause nobody proved. Acceptance: the four forms print their shift; the test's expected lines cover a shifted case of each; the comment is proven by a reduced program and kept with its fact, or removed. Files: the arm64 disassembler package (dot habu-give-the-arm64-fa066b7f names it), tools/imagedisasm.f, its test. Verify: the disassembler test on a built hb. Depends: habu-the-arm64-disassembler-876daec0 lands first. Ownership: the disassembler files. Claim: unassigned.
