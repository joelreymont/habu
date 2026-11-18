---
title: Name where a build gave back data space
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T20:19:25.247025+01:00"
---

Problem: src/habu/linker.f END-CK rejects a build in which DP moved back with "linker: FILE: it gave back data space (a caught failure or a negative allot), so the build writes nothing", naming only the build's file: not the line of the caught evaluate or the negative allot, nor a required file the rollback gave back. Every rollback past a load ends this way, since MARKS and PIECE-END each allot (POINTER-STORAGE:ALIGN-APART), even when the load raised the data floor (a 600 KB literal in the required file). test/fixtures/file-objects/rollback.f FOB-REQ is that shape. Acceptance: the rejection names the file and line where DP was lowered and, when the rollback gave back a loaded file, that file, in the form of the other rejections (place, source line, what to change); CAUGHT-CASE in test/file-objects.f checks it. Depends: none. Claim: agent=jerry (design: /tmp/jerry-gbp-design-brief.md).
