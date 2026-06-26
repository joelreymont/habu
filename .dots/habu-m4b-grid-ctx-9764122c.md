---
title: "M4b: GRID-CTX and ROW-CTX (distinct, no overload)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:11:55.472166+02:00"
blocks:
  - habu-m4a-trusted-span-39398657
---

Decomposes M4. Define GRID-CTX ( span<S,T,N> -- gridctx<B,N,M> ) flat grid-strided ctx and ROW-CTX ( span<S,T,N> -- rowctx<B,N,M> ) row-local ctx. DISTINCT words, no overload (Resolved-M1/M2 #4). The ctx carries lane index + mask token only; the span carries the base. Mask token lives on the ctx (Resolved-M1/M2 #2: gridctx<B,N,M>).
- Files: lib/ptx-tile.f.
- Verify: GRID-CTX output extent token equals the span extent (proven); using a rowctx where a gridctx is required rejects.
- Dep: M4a.
