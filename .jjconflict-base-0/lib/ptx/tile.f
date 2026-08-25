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
\ the phantom-MINTING contexts and loads are TRUSTED: boundaries whose declared
\ effect is the contract. The phantom-CONSUMING stores return
\ nothing, so they are CHECKED callers of the PTXREP:SINK* combinators
\ (lib/ptx/rep.f) — the operand registers flow through the ( n … -- ) emitter and
\ no phantom is minted. Kernels are CHECKED here, not run.
\
\ BOUNDARY (named, tested). The checker proves space / extent / mask AGREEMENT by
\ shared token. Context producers mint a fresh rigid mask per call, so independently
\ derived masks are distinct unless an explicit constructor stamps the same token.
\ Span extent freshness is the same checker mechanism for MK-SPAN-style
\ constructors; fixed kernel-ABI spans may still assert a named extent token.
\ Retirement owner for every trusted site below:
\ habu-ptx-phantom-preserving-3df9db92.
\ Load after lib/errors.f.

require lib/ptx/rep.f

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

\ LOAD / LOAD-ONCE REPACKAGE span+gridctx registers into a tile (element from the
\ span, block+mask from the ctx). No fresh phantom is minted, so they are CHECKED
\ callers of PTXREP:MINT-LOAD (lib/ptx/rep.f): the projection is pinned by the
\ combinator's declared types and the checked-mint provenance seal rejects any
\ free-typed forge. Byte-identical to the former TRUSTED: EMIT-LOAD rows.
: LOAD ( span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   [: EMIT-LOAD ;] PTXREP:MINT-LOAD ;

: LOAD-ONCE ( span<space-global-once,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   [: EMIT-LOAD-ONCE ;] PTXREP:MINT-LOAD ;

: STORE ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- )
   [: EMIT-STORE ;] PTXREP:SINK3 ;

: STORE-ONCE ( tile<t,b,m> span<space-global-once,t,e> gridctx<b,e,m> -- )
   [: EMIT-STORE-ONCE ;] PTXREP:SINK3 ;

: SCATTER-ADD ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- )
   [: EMIT-SCATTER-ADD ;] PTXREP:SINK3 ;

TRUSTED: FANIN-CTX ( ptr<space-global,t> -- fanctx<b,extent-n,fresh-mask-live> )
   EMIT-FANIN-CTX ;

TRUSTED: FANIN-LOAD ( ptr<space-global,t> fanctx<b,e,m> -- tile<t,b,m> )
   EMIT-FANIN-LOAD ;

: FANIN-SCATTER-ADD ( tile<t,b,m> ptr<space-global,t> fanctx<b,e,m> -- )
   [: EMIT-FANIN-SCATTER-ADD ;] PTXREP:SINK3 ;

TRUSTED: INDEX-CTX ( span<space-global,u32,i> span<space-global,t,e> -- idxctx<b,i,e,fresh-mask-live> )
   EMIT-INDEX-CTX ;

TRUSTED: UNIQUE-INDEX-CTX ( span<space-global,u32,i> span<space-global,t,e> -- uniqidxctx<b,i,e,fresh-mask-live> )
   EMIT-UNIQUE-INDEX-CTX ;

TRUSTED: INDEX-DENSE-LOAD ( span<space-global,t,i> idxctx<b,i,e,m> -- tile<t,b,m> )
   EMIT-INDEX-DENSE-LOAD ;

TRUSTED: UNIQUE-INDEX-DENSE-LOAD ( span<space-global,t,i> uniqidxctx<b,i,e,m> -- tile<t,b,m> )
   EMIT-UNIQUE-INDEX-DENSE-LOAD ;

: INDEX-DENSE-STORE ( tile<t,b,m> span<space-global,t,i> idxctx<b,i,e,m> -- )
   [: EMIT-INDEX-DENSE-STORE ;] PTXREP:SINK3 ;

TRUSTED: INDEX-LOAD ( span<space-global,u32,i> span<space-global,t,e> idxctx<b,i,e,m> -- tile<t,b,m> )
   EMIT-INDEX-LOAD ;

: INDEX-SCATTER-ADD ( tile<t,b,m> span<space-global,u32,i> span<space-global,t,e> idxctx<b,i,e,m> -- )
   [: EMIT-INDEX-SCATTER-ADD ;] PTXREP:SINK4 ;

: INDEX-STORE ( tile<t,b,m> span<space-global,u32,i> span<space-global,t,e> uniqidxctx<b,i,e,m> -- )
   [: EMIT-INDEX-STORE ;] PTXREP:SINK4 ;

\ Phantom-preserving pointwise ops: CHECKED callers of the PTXREP register-emitter
\ combinators (lib/ptx/rep.f). The operand tile's register flows through the
\ ( n … -- n ) emitter and the SAME tile phantom is returned, so these certify
\ instead of asserting a TRUSTED: boundary. FMA.'s result tile is the MIDDLE
\ operand (uniform ∘ tile ∘ tile -> tile), so it preserves the SECOND phantom
\ through REPMIX3B.
: SCALE ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   [: EMIT-SCALE ;] PTXREP:REPMIX2 ;

: FMA. ( uniform<t> tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-FMA ;] PTXREP:REPMIX3B ;

: +. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-ADD ;] PTXREP:REP2 ;

: -. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-SUB ;] PTXREP:REP2 ;

: *. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-MUL ;] PTXREP:REP2 ;

: /. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-DIV ;] PTXREP:REP2 ;

: RELU ( tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-RELU ;] PTXREP:REP1 ;
