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
\ BOUNDARY (named, tested; capability dotted). The checker proves space / extent /
\ mask AGREEMENT by shared token, but cannot yet prove two INDEPENDENTLY derived
\ extent or mask tokens are DISTINCT (polymorphic vars unify freely), so a
\ mixed-mask program is not yet rejected. The fix is per-call fresh RIGID extent/
\ mask token minting at the ctx/span producers (dot
\ habu-add-per-call-... "fresh rigid extent-token minting"). Until then the
\ space-wrong and missing-ctx negatives hold; the mask/extent-identity negatives
\ do not. Load after lib/errors.f.

TRUSTED: GRID-CTX ( span<space-global,t,e> -- gridctx<b,e,m> )
   EMIT-GRID-CTX ;

TRUSTED: LOAD ( span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   EMIT-LOAD ;

TRUSTED: STORE ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- )
   EMIT-STORE ;

TRUSTED: SCALE ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   EMIT-SCALE ;

TRUSTED: +. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-ADD ;

TRUSTED: *. ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-MUL ;

TRUSTED: RELU ( tile<t,b,m> -- tile<t,b,m> )
   EMIT-RELU ;
