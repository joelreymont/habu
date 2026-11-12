---
title: "Reserve a vector's room so later growth allocates nothing"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T14:12:21.795891+02:00"
---

Problem: src/lib/vector.f has no way to reserve room. ENSURE (:96) and PUSH (:111) say the block may move, with no condition; only the header's 'growth doubles the room' implies that a PUSH below the room allocates nothing. Maki needs that guarantee: naming a part's faces must allocate nothing once the kernel has built a shape, or a throw leaks the shape (maki src/part.f TRIM-BODY, KEEP, SHELL-THROUGH, EXTRUDE). Maki's ORDINAL-ROOM imitates a reserve in two calls: ENSURE to the reserved count, then RESIZE back down. Acceptance: VEC:RESERVE ( ptr vector<t> n -- ) makes room for n elements (n checked as ENSURE checks it), and until COUNT passes n, ENSURE, RESIZE and PUSH neither allocate nor move the block. The words' comments state this, and a test shows it. Depends: none. Claim: unassigned (Habu owner carl).
