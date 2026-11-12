---
title: Grow the typed vector on an allocator
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T10:31:03.600305+02:00"
---

Problem (tim, Maki lead, 2026-11-12; /tmp/tim-vec-grow.md): a VEC:vector<t> cannot grow: after 'al 4 [type] n VEC:NEW' no word makes index 4 valid (src/lib/vector.f has NEW, COUNT, AT, DELETE for the typed vector; PUSH, RESIZE, ENSURE are the cell vector's). Maki's sessions (release 2026-11-15) hold OPS's key, text and reference stores as vectors in each open design's session arena, grown by PREPARE (maki src/ops.f KEYS-RESERVE, N-REF-AT-RESERVE). Acceptance: a typed vector's count grows to at least n on ARENA:ALLOCATOR, elements kept, new ones zero, amortized; the old storage stays until the arena is released; Maki's reproducer indexes the grown vector. Order (Joel 2026-11-12, interview, the chosen option: "Finish the move first (Recommended)", "Heap allocator, move every caller, delete the cell vector. Maki's sessions likely miss Sunday."): the typed vector takes PUSH, RESIZE and ENSURE only after the heap allocator exists (habu-heap-maps-a-657c7988), every VEC:INIT caller has moved (habu-move-vec-s-4edbc913; Zen 9, Tender 20, Habu 27 vectors) and the cell vector is deleted; no renamed or second set of growth words. Steps: the allocator's resize (ALLOC:RESIZE, the arena's resize); the heap allocator; the callers' move and the cell vector's deletion; the typed vector's growth words. Depends: habu-heap-maps-a-657c7988, habu-move-vec-s-4edbc913. Claim: agent=carl workspace=.jj-ws.noindex/carl-alloc (the allocator's resize).
