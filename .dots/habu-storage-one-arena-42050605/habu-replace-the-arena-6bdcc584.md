---
title: Replace the ARENA NAME t and HEAP NAME t definers with the generic making words
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:44:44.959556+02:00"
---

Problem: src/core/arena.f's ARENA NAME t and src/core/heap.f's HEAP NAME t generate NAME-NEW per declaration. Settled (parent, point 4): both definers go. Callers to cut over in one install: habu src/lib/net/mcp.f; zen src/jobs/worker.f, src/document/distill-pdf.f, src/document/outline.f, src/sql.f; maki 16 ARENA declarations (sketch-constraints.f, sheet.f, session.f, pattern.f, trim.f, weld-solid.f); tender 4 ARENA and 2 HEAP lines. Acceptance: no caller of either definer in the four repositories (rg -i --no-ignore); the definers, their checker rows and verify-source rows deleted; the three consumers build cold and warm equal. Files: src/core/arena.f, src/core/heap.f, src/core/checker.f, src/habu/verify-source.f, src/habu/interpreter.f, the callers. Verify: suite; consumers cold and warm. Depends: the allocator child; HEAP's part also the system-heap dot habu-heap-maps-a-657c7988. Ownership: carl. Claim: unassigned.
