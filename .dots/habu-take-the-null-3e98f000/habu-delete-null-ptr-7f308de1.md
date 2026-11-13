---
title: Delete NULL-PTR and refuse zero-filled pointer cells
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:27:45.380668+02:00"
blocks:
  - habu-convert-habu-s-4e04346b
---

Lane 3 of /home/joel/Work/habu/.jj-ws.noindex/jerry-wasm/.out/design-null-reach.md: delete NULL-PTR and the checker's null kind (TVK-NULL, A-NULL, PE-PTR-A-NULL and their arms); TFAM-STORE-WALK refuses a ptr in storage that starts zero and can be read first (rule Z); FUNCTION: refuses a ptr result; the ptr-output primitive rows audited for a zero answer. Acceptance: the null-read hunt's 22 programs (their files are lost; habu-take-the-null-3e98f000 gives two of their kind), and `require src/lib/arena.f  TYPED-VARIABLE A ARENA:arena  : USE ( -- ) A 8 ARENA:ALLOC drop c@ . ;  USE` and `require src/lib/vector.f  TYPED-VARIABLE V VEC:vector<n>  V VEC:DELETE`, refused with a named code; the full suite; the chain check. Depends: lane 2. Claim: unassigned.
