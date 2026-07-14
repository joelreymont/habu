---
title: Reclaim resolved ARM64 fixups
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-14T04:22:51.934731+02:00\""
---

Full context: src/arch/arm64/icode.f uses a fixed 0x1000 fixup table, and LBL, patches matching rows without retiring them. NFX therefore counts all historical forward references, label definition scans all historical rows, and the owner-persistence child fails closed after refresh with 'icode: out of fixups' / wrapper rc67 despite most rows already being resolved. Root fix: compact matched rows in place when a label resolves so NFX is exactly the simultaneous pending-fixup count; keep a real pending-set bound and the exact overflow diagnostic. Acceptance: a checked regression emits and resolves more than 4096 sequential forward references without overflow, verifies every patched instruction, proves NFX returns to zero, proves unrelated pending rows survive compaction, and proves the true simultaneous-pending edge fails with 'icode: out of fixups'; focused assembler suite and owner-persistence child pass. Files: src/arch/arm64/icode.f, focused test/manifest, FILEMAP.md, LESSONS.md. Dependency: blocks habu-owner-seal-persist-1f23e205.

Claim: agent=/root/icode_reclaim workspace=.jj-ws/icode-reclaim.
