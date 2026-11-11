---
title: Every arena maps its own 16 KiB first chunk, child arenas included
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:09:54.440504+02:00"
---

Problem (Joel 2026-11-11: 'why in the world are we shooting ourselves in the foot like that?', 'this is a self-imposed limitation'): src/lib/arena.f:40 FIRST-BYTES $4000; NEW (:169) maps it from the OS and NEW-IN (:177) calls NEW, so every arena, a child too, costs one OS mapping of at least a page (16 KiB on arm64 macOS, hw.pagesize 16384). Nothing requires an arena that lives inside a parent to own a mapping. Acceptance: a child arena takes its chunks from its parent, so many small arenas under one parent cost a header each; a root arena still maps. Files: src/lib/arena.f, src/lib/arena-test.f. Verify: a test making 1000 child arenas maps no more than the bytes they hold. Depends: the arena's shape (typed or mixed), dot habu-give-an-arena-25a7d3cc. Ownership: carl. Claim: unassigned.
