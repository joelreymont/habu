---
title: Grow the typed vector on an allocator
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T10:31:03.600305+02:00"
---

Problem (tim, Maki lead, 2026-11-12; /tmp/tim-vec-grow.md): a VEC:vector<t> cannot grow: after 'al 4 [type] n VEC:NEW' no word makes index 4 valid (src/lib/vector.f has NEW, COUNT, AT, DELETE for the typed vector; PUSH, RESIZE, ENSURE are the cell vector's). Maki's sessions (release 2026-11-15) hold OPS's key, text and reference stores as vectors in each open design's session arena, grown by PREPARE (maki src/ops.f KEYS-RESERVE, N-REF-AT-RESERVE). Acceptance: a typed vector's count grows to at least n on ARENA:ALLOCATOR, elements kept, new ones zero, amortized; the old storage stays until the arena is released; Maki's reproducer indexes the grown vector. Steps: the allocator's resize (ALLOC:RESIZE, the arena's resize); the vector's growth words. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-alloc (the allocator's resize).
