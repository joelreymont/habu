\ ptx-tile.f - PTX tile-DSL v0 operation vocabulary (M4).
\
\ The checked tile operations a kernel body composes: grid-context derivation,
\ masked load/store, and tile arithmetic. Types are the M2 parametric system
\ (span / gridctx / tile / uniform). Element `t`, extent `e`, block `b`, and mask
\ `m` are polymorphic type variables that thread by unification, so a kernel body
\ type-checks end to end (a checked SAXPY in ptx-tile-test.f certifies). NB `n` is
\ the reserved generic-int token, never an extent var - use `e`.
\
\ These are PTX PRIMITIVES: the checker cannot infer a body that lowers to PTX, so
\ each is a TRUSTED: boundary whose declared effect is the contract (see
\ TRUSTED.md). Bodies throw E-PTX-NOIMPL until codegen lands (M4e) - kernels are
\ CHECKED here, not run.
\
\ BOUNDARY (named, tested). The checker proves space / extent / mask AGREEMENT by
\ shared token. Context producers mint a fresh rigid mask per call, so independently
\ derived masks are distinct unless an explicit constructor stamps the same token.
\ Span extent freshness is the same checker mechanism for MK-SPAN-style
\ constructors; fixed kernel-ABI spans may still assert a named extent token.
\ Load after lib/errors.f.

TRUSTED: MK-SPAN ( ptr<space-global,t> u32 -- span<space-global,t,fresh-extent-n> )
   drop ;

TRUSTED: MK-SPAN-ONCE ( ptr<space-global,t> u32 -- span<space-global-once,t,fresh-extent-n> )
   drop ;

TRUSTED: MK-SPAN= ( ptr<space-global,t> ptr<space-global,u> u32 -- span<space-global,t,fresh-extent-n> span<space-global,u,fresh-extent-n> )
   drop ;

TRUSTED: MK-MATRIX ( ptr<space-global,t> u32 u32 -- matrix<space-global,t,fresh-extent-r,fresh-extent-c> )
   drop drop ;

TRUSTED: MK-MATRIX-ONCE ( ptr<space-global,t> u32 u32 -- matrix<space-global-once,t,fresh-extent-r,fresh-extent-c> )
   drop drop ;

TRUSTED: GRID-CTX ( span<space-global,t,e> -- gridctx<b,e,fresh-mask-live> )
   EMIT-GRID-CTX ;

TRUSTED: GRID-CTX-ONCE ( span<space-global-once,t,e> -- gridctx<b,e,fresh-mask-live> )
   EMIT-GRID-CTX-ONCE ;

TRUSTED: COOP-CTX ( span<space-global,t,e> -- coopctx<b,e,fresh-mask-live> )
   EMIT-COOP-CTX ;

TRUSTED: LOAD ( span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   EMIT-LOAD ;

TRUSTED: LOAD-ONCE ( span<space-global-once,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   EMIT-LOAD-ONCE ;

TRUSTED: STORE ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- )
   EMIT-STORE ;

TRUSTED: STORE-ONCE ( tile<t,b,m> span<space-global-once,t,e> gridctx<b,e,m> -- )
   EMIT-STORE-ONCE ;

TRUSTED: SCATTER-ADD ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- )
   EMIT-SCATTER-ADD ;

TRUSTED: FANIN-CTX ( ptr<space-global,t> -- fanctx<b,extent-n,fresh-mask-live> )
   EMIT-FANIN-CTX ;

TRUSTED: FANIN-LOAD ( ptr<space-global,t> fanctx<b,e,m> -- tile<t,b,m> )
   EMIT-FANIN-LOAD ;

TRUSTED: FANIN-SCATTER-ADD ( tile<t,b,m> ptr<space-global,t> fanctx<b,e,m> -- )
   EMIT-FANIN-SCATTER-ADD ;

TRUSTED: SCALE ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   EMIT-SCALE ;

TRUSTED: FMA. ( uniform<t> tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-FMA ;

TRUSTED: +. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-ADD ;

TRUSTED: -. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-SUB ;

TRUSTED: *. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-MUL ;

TRUSTED: /. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-DIV ;

TRUSTED: RELU ( tile<t,b,m> -- tile<t,b,m> )
   EMIT-RELU ;
