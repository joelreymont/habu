---
title: "Move VEC's callers onto the typed vector"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T20:44:44.975793+02:00"
closed-at: "2026-11-12T12:30:24.219860+02:00"
---

Problem: VEC (src/lib/vector.f) is a growable cell vector that maps its own storage; settled (parent, point 5): it becomes the one typed vector on any allocator. Callers: 55 vectors in 15 files: habu 28 (src/lib/source-lex.f 11, word-docs.f 2, process-command.f 7, spawn.f 3, tools/lint/intern.f 5), zen 6 (src/document/cfb.f 1, cfb-write.f 1, src/jobs/worker.f 4), tender 21 (tenderd/classify.f 2, api.f 8, graph.f 2, edits.f 3, src/questions/semantic.f 2, src/admission.f 3, src/table.f 1). Acceptance: every caller makes its vector from an allocator and states its element type; no second vector package. Files: src/lib/vector.f, callers. Verify: suite; consumers cold and warm. Depends: none; lands in one cutover with habu-grow-the-typed-54653413 (the growth words take the cell vector's names). Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-vec.
