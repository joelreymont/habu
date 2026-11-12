---
title: Replace the ARENA NAME t and HEAP NAME t definers with the generic making words
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:44:44.959556+02:00"
---

Problem: src/core/arena.f's ARENA NAME t and src/core/heap.f's HEAP NAME t generate NAME-NEW per declaration. Settled (parent, point 4): both definers go. Callers to cut over in one install: habu src/lib/net/mcp.f; zen src/jobs/worker.f, src/document/distill-pdf.f, src/document/outline.f, src/sql.f; maki 16 ARENA declarations (sketch-constraints.f, sheet.f, session.f, pattern.f, trim.f, weld-solid.f); tender 4 ARENA and 2 HEAP lines. NAME-NEW-DISPOSE has 16 callers on maki master (tim 2026-11-12, .out/tim-l4b-carl.md; `rg -c NEW-DISPOSE src`: assembly.f 3, part.f 3, model.f 2, sketch.f 2, trim.f 2, drawing.f 1, sheet.f 1, sheet-formed.f 1, views.f 1; first reached at src/views.f:160 when a view registers during a build) and none in zen or tender. ALLOC:NEW (src/lib/allocator.f) takes no release action and ARENA:ALLOC-DISPOSE (src/lib/arena.f) answers bytes each package would cast, so the cutover needs a typed word that makes a record with a release action: designed first, its public name to Joel. Acceptance: that word exists and Maki's 16 sites call it with no cast of their own; no caller of either definer in the four repositories (rg -i --no-ignore); the definers, their checker rows and verify-source rows deleted; the three consumers build cold and warm equal. Files: src/core/arena.f, src/core/heap.f, src/core/checker.f, src/habu/verify-source.f, src/habu/interpreter.f, the callers. Verify: suite; consumers cold and warm. Depends: none (the heap allocator is src/lib/heap.f). Ownership: carl. Claim: unassigned.
