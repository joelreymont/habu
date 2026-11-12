---
title: Copy bytes into an arena in one word
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T14:12:21.808817+02:00"
---

Problem: Maki copies n bytes into an arena by hand in about ten places (maki src/build.f SCRATCH-COPY, model.f KEEP$, views.f, weld.f, part.f, sketch.f, sheet.f, assembly.f, param.f, sketch-constraints.f). Each needs its own zero case, because ARENA:ALLOC rejects an empty request (E-MEM-SIZE, src/lib/arena.f ALLOC). Acceptance: one ARENA word, for example ARENA:COPY ( ptr arena ptr u8 n -- ptr u8 ), answers a copy of n bytes in the arena, or NULL-PTR when n is 0. Its comment states this and a test shows it. Depends: none. Claim: unassigned (Habu owner carl).
