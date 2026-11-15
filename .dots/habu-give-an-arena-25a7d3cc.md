---
title: Give an arena fixed-length typed arrays
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:42:59.654895+02:00"
---

Problem (Maki, tim 2026-11-11, request kept at .out/tim-arena-arrays.md): ARENA:ALLOC ( ptr ARENA:arena n -- ptr u8 n ) answers bytes; reading them as cells or reals needs a private ptr u8 -> ptr n cast in every Maki package. Callers: maki src/trim.f (each surface, curve, vertex and edge keeps a fixed run of 4 to 15 value cells, today each record's own vals field, a VEC:vector<n> (:70-169; PUT :483, DECLARE :501)) and src/assembly.f (an instance holds pose[12], shift[3], shift-err[3] reals and 24 turn rows of four columns :108-113, :260). Needed: n zeroed elements of a stored type from a given arena, the count fixed at the allocation, indexed with a bounds check, released with the arena. Gates Maki TRIM C3 and ASSEMBLY C2. Acceptance: maki src/trim.f and src/assembly.f hold their runs through the one typed vector (src/lib/vector.f VEC:NEW over ARENA:ALLOCATOR) with no cast of their own; Maki builds cold and warm. Depends: none. Ownership: carl. Claim: unassigned.
