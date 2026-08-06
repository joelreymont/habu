\ once-cg.f - emit checked once-space PTX load/store coverage.
\
\ A `space-global-once` span is the typed read-once/affine gradient-buffer
\ witness. LOAD-ONCE's adjoint may use STORE-ONCE; this emitter proves the
\ once path lowers to ordinary ld/st, not red.global.add. Load after lib/ptx/cg.f,
\ lib/ptx/header.f, and lib/ptx/tile.f. Emits to stdout.
\
\ The kernel locals stay BARE. Every type a kernel body binds - span, gridctx -
\ is a PARAMETRIC family, and a `{: x:fam<..> :}` annotation is fail-closed in the
\ locals parser (docs/type-families.md 17.1; the capability is dot
\ habu-typed-locals-for-b06b6707). A single-letter annotation such as `x:a` is not
\ a substitute: it DECLARES a fresh quantifier `a` for this word, which the body
\ then specializes to span - a false parametricity claim the checker rejects with
\ E-NONPARAMETRIC-EFFECT.

package PTXONCECG

256 %BLOCK

KERNEL: ONCE-SPAN ( span<space-global-once,f32,extent-n> -- )  GRID: ceil-n-256
   {: x :}                     \ typed-local-lint: allow-bare-local - parametric kernel type
   x GRID-CTX-ONCE {: g :}     \ typed-local-lint: allow-bare-local - parametric kernel type
   x g LOAD-ONCE
   x g STORE-ONCE ;

: CG-ONCE-ENTRY ( -- )
   s" .visible .entry ONCE_SPAN(.param .u64 p_x, .param .u32 p_n)" PTX-L ;

: CG-ONCE-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u32 %r1, [p_n];" PTX-L ;

: EMIT-ONCE ( -- )
   CG-RESET  CG-HEADER CG-ONCE-ENTRY CG-OPEN CG-ONCE-PARAMS
   1 SPAN-ONCE-REG  ONCE-SPAN
   CG-RET CG-CLOSE ;

EMIT-ONCE

;package
