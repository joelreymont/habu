\ tile-v4a-test.f - positive proof for the M10 typed, alignment-proven vec4 vocabulary.
\
\ Three parts:
\  1. The KERNEL: definitions ARE the positive proof: SAXPY-V4A / RELU-SPAN-V4A /
\     SUBDIV-SPAN-V4A each thread a plain span through V4-ALIGN into a vspan, derive a
\     v4 gridctx, and compose LOAD.V4 / STORE.V4 / SCALE.V4 / ADD.V4 / RELU.V4 / SUB.V4 /
\     DIV.V4 / MUL.V4 over vtile<>. A clean load certifies the typed surface end to end.
\  2. PTXT emit regression: SAXPY-V4A lowers to the ld.global.v4.f32 body PLUS the
\     @%p-guarded scalar residual tail for BOTH the load and the store - the M10 (c)
\     scalar-residual tail effect, tracked by the mask m on vtile and materialised by the
\     shared cg-vec.f emit.
\  3. Byte-identity: the typed SAXPY-V4A emits PTX byte-for-byte identical to the legacy
\     hyphenated SAXPY-V4 (tile-v4.f), proving the typed layer is a PURE checker
\     refinement - no codegen change - so its device correctness is the already-passing
\     saxpy-v4-tail-device-test.f cubin.
\
\ The negatives (misaligned base, unproven store base, wrong lane arity) are gated
\ separately by lib/ptx/tile-v4a-neg-test.f. Load after lib/ptx/tile.f + tile-v4.f + tile-v4a.f.

require lib/ptx/test-prelude.f
require lib/ptx/tile-v4a.f

T-RESET

256 %BLOCK

\ ---- part 1: the typed vec4 kernels certify (positive proof) --------------------
KERNEL: SAXPY-V4A ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-1024
   {: x y a :} \ typed-local-lint: allow-bare-local - generic PTX param types contain commas.
   x GRID-CTX.V4 {: g :} \ typed-local-lint: allow-bare-local - checked gridctx type is inferred.
   x V4-ALIGN {: xa :} \ typed-local-lint: allow-bare-local - checked vspan type is inferred.
   y V4-ALIGN {: ya :} \ typed-local-lint: allow-bare-local
   xa g LOAD.V4  a SCALE.V4
   ya g LOAD.V4  ADD.V4
   ya g STORE.V4 ;

KERNEL: RELU-SPAN-V4A ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-1024
   {: s :} \ typed-local-lint: allow-bare-local - generic PTX param types contain commas.
   s GRID-CTX.V4 {: g :} \ typed-local-lint: allow-bare-local - checked gridctx type is inferred.
   s V4-ALIGN {: sa :} \ typed-local-lint: allow-bare-local - checked vspan type is inferred.
   sa g LOAD.V4 RELU.V4
   sa g STORE.V4 ;

KERNEL: SUBDIV-SPAN-V4A ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-1024
   {: x y :} \ typed-local-lint: allow-bare-local - generic PTX param types contain commas.
   x GRID-CTX.V4 {: g :} \ typed-local-lint: allow-bare-local - checked gridctx type is inferred.
   x V4-ALIGN {: xa :} \ typed-local-lint: allow-bare-local - checked vspan type is inferred.
   y V4-ALIGN {: ya :} \ typed-local-lint: allow-bare-local
   xa g LOAD.V4  ya g LOAD.V4  SUB.V4
   ya g LOAD.V4  DIV.V4
   ya g LOAD.V4  MUL.V4
   ya g STORE.V4 ;

\ ---- the hyphenated legacy twin, for the byte-identity proof ---------------------
KERNEL: SAXPY-V4H ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-1024
   {: x y a :} \ typed-local-lint: allow-bare-local - generic PTX param types contain commas.
   x GRID-CTX-V4 {: g :} \ typed-local-lint: allow-bare-local - checked gridctx type is inferred.
   x g LOAD-V4  a SCALE-V4
   y g LOAD-V4  ADD-V4
   y g STORE-V4 ;

: EMIT-TYPED ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  SAXPY-V4A
   CG-RET CG-CLOSE ;

: EMIT-HYPH ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  SAXPY-V4H
   CG-RET CG-CLOSE ;

$4000 constant V4A-CAP                 \ SAXPY v4 PTX is ~2 KB; one capture fits
create V4A-GOLD V4A-CAP allot          \ typed capture, survives the next capture
variable V4A-GOLD-U

: V4A-GOLD! ( ptr u8 n -- ) {: a:ptr u:n :}
   u V4A-CAP > if s" tile-v4a-test: capture overflow" 76 die then
   a V4A-GOLD u BYTE-COPY  u V4A-GOLD-U ! ;

: V4A-GOLD$ ( -- ptr u8 n )  V4A-GOLD V4A-GOLD-U @ ;

: V4A-HAS ( ptr u8 n -- )   \ assert the typed capture contains a line
   V4A-GOLD$ 2swap CONTAINS? TTRUE ;

: V4A-NOT-HAS ( ptr u8 n -- )
   V4A-GOLD$ 2swap CONTAINS? 0= TTRUE ;

: V4A-MAIN ( -- )
   \ part 2: capture the TYPED emit and pin the v4 body + masked scalar tail
   PTX-CAPTURE-ON EMIT-TYPED PTX-CAPTURE-OFF  PTX-CAPTURE$ V4A-GOLD!
   s" typed emit is non-empty" T-LABEL  V4A-GOLD-U @ 0 > TTRUE
   s" v4 body load"        T-LABEL  s" ld.global.v4.f32 {%f2, %f3, %f4, %f5}, [%rd4];" V4A-HAS
   s" masked tail load L0"  T-LABEL  s" @%p3 ld.global.f32 %f2, [%rd4];" V4A-HAS
   s" masked tail load L3"  T-LABEL  s" @%p6 ld.global.f32 %f5, [%rd7];" V4A-HAS
   s" scale broadcast"      T-LABEL  s" mul.rn.f32 %f6, %f1, %f2;" V4A-HAS
   s" elementwise add"      T-LABEL  s" add.rn.f32 %f14, %f6, %f10;" V4A-HAS
   s" v4 body store"        T-LABEL  s" st.global.v4.f32 [%rd14], {%f14, %f15, %f16, %f17};" V4A-HAS
   s" masked tail store L0" T-LABEL  s" @%p13 st.global.f32 [%rd14], %f14;" V4A-HAS
   s" masked tail store L3" T-LABEL  s" @%p16 st.global.f32 [%rd17], %f17;" V4A-HAS
   s" no emit ERROR"        T-LABEL  s" ERROR" V4A-NOT-HAS
   \ part 3: typed emit is byte-identical to the legacy hyphenated emit
   PTX-CAPTURE-ON EMIT-HYPH PTX-CAPTURE-OFF  PTX-CAPTURE$ {: ha:ptr hu:n :}
   s" typed PTX == hyphenated PTX (pure type refinement)" T-LABEL
   ha hu V4A-GOLD$ T-STR= TTRUE ;

V4A-MAIN

\ Clean load past this point is the positive proof: the typed vec4 vocabulary certifies,
\ emits the v4 body + masked residual tail, and lowers byte-identically to the codegen
\ the device test already validates.

T-REPORT
