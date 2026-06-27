---
title: "M4c: LOAD/STORE typed (distinct grid/row)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:11:55.479213+02:00"
blocks:
  - habu-m4b-grid-ctx-9764122c
---

Decomposes M4. Define LOAD ( span<G,T,N> gridctx<B,N,M> -- tile<T,B,M> ) and STORE ( tile<T,B,M> span<G,T,N> gridctx<B,N,M> -- ), plus ROW-LOAD/ROW-STORE for rowctx (distinct words). Mask token M threads from ctx to tile. Inactive lanes poison (no magic zero).
- Files: lib/ptx/tile.f.
- Verify: ctx extent must equal span extent (mismatch rejects); LOAD then STORE round-trips the mask token; a global LOAD on a non-global span rejects.
- Dep: M4b.
