---
title: Grow the typed vector on an allocator
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T10:31:03.600305+02:00"
---

Problem (tim, Maki lead, 2026-11-12; .out/tim-l4b-carl.md): a VEC:vector<t> cannot grow: after 'al 4 [type] n VEC:NEW' no word makes index 4 valid (src/lib/vector.f has NEW, COUNT, AT, DELETE for the typed vector; PUSH, RESIZE, ENSURE are the cell vector's). Maki's first callers of growth (release 2026-11-15): TRIM's kernel pack (maki src/trim.f PACK-PUT: `PACK-LEN @ 1+ PACK-RESERVE`, handed whole to the kernel as one span); OPS's element rows (maki src/ops.f EL!: seven columns, `i 1+ ...-RESERVE` each); the kernel record's request (maki src/kernel.f REQUEST-RESERVE, which doubles by hand; Maki dot maki-grow-the-kernel-4abff972). They need PUSH of one element and ENSURE to a count, on ARENA:ALLOCATOR only. Acceptance: a typed vector's count grows to at least n on ARENA:ALLOCATOR, elements kept, new ones zero, amortized; the old storage stays until the arena is released; Maki's reproducer indexes the grown vector. Order (Joel 2026-11-12, interview, the chosen option: "Finish the move first (Recommended)", "Heap allocator, move every caller, delete the cell vector. Maki's sessions likely miss Sunday."): the typed vector takes PUSH, RESIZE and ENSURE only after every VEC:INIT caller has moved (habu-move-vec-s-4edbc913; Habu 28, Zen 6, Tender 21 vectors) and the cell vector is deleted; no renamed or second set of growth words. Steps: one cutover, since the growth words take the cell vector's names: the cell vector's deletion with the typed vector's growth words, then each caller file's move, landing whole. Depends: habu-move-vec-s-4edbc913 (the same landing). Claim: agent=carl workspace=.jj-ws.noindex/carl-vec.
