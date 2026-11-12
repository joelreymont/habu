---
title: "Storage: one arena, one allocator record, typed words made once"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:44:44.906332+02:00"
---

Settled by Joel 2026-11-11 ('dot and do the above then. i agree with this', of these six points as shown to him): 1. An arena is one reserved range with a bump pointer. No chunks and no arena-inside-a-parent mechanism. A child arena keeps only its meaning: it is released with its parent. 2. An allocator is a small record that the arena and the heap each provide. Containers take it when made and keep it; their type does not mention it. 3. The heap is the system allocator on macOS and Linux. The page per HEAP record goes. 4. Typed things are made by one set of generic words using [type]. The ARENA NAME t and HEAP NAME t definers go, and so do the planned ARENA:VECTOR and ARENA:RECORD. 5. There is one vector: today's VEC becomes that one typed vector on any allocator. 6. Wasm waits for a fact: whether anything built for it uses an arena; checked before anything is designed there. Earlier, his: '[type]' names the form that states a stored type and width<t> is its value's type; ARENA:COUNT counts elements; 'keep the rest of the api matching if you can'. Open, not blocking: where linear types apply (things made in an arena stay plain pointers). Public names (the allocator, the making words, the vector's words, an arena's reserved size) are Joel's, asked one at a time as each child's design names them. Consumer path: Maki's TRIM C3 and ASSEMBLY C2 wait on fixed-length typed arrays in its arenas (dot habu-give-an-arena-25a7d3cc); children 1 and 2 unblock it, the rest sit behind the allocator record and do not change Maki's code again. Campaign dot: work is in the children. Ownership: carl.

Point 6 checked (2026-11-11): maki viewer-main.f's require closure is 36 files and src/lib/arena.f is not among them (ARENA is named only by error.f, xref.f and layout.f, none a use). Nothing built for Wasm uses an arena, so no arena is designed for Wasm; the heap there is decided when a Wasm program needs one.
