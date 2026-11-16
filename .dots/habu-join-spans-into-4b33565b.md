---
title: Join spans into an arena
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T16:08:46.422470+01:00"
---

Problem: Habu has no word that copies spans into an arena as one span (src/lib/arena.f, span.f, string.f and allocator.f have none), so Maki writes its own: STORAGE:JOINED$ (maki src/storage.f:71-78) joins three spans in the task's scratch arena for every rejection and face name it composes, and maki src/ops.f ANSWER$, src/project-import.f STEP-COPY$ and src/assembly.f KEEP-TEXT each copy one span into an arena by hand. Joining spans into an arena is generic string support, which belongs in Habu. Acceptance: a Habu word copies one or more spans into a given arena and answers them as one span; Maki's STORAGE:JOINED$ and its single-span copies can call it. Depends: none. Claim: unassigned.
