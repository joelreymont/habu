\ ptx-tile-test.f - checked SAXPY kernel over the M4 tile vocabulary.
\
\ The KERNEL: definition IS the positive proof: it certifies against its declared
\ parametric effect (a reject would emit a checker diagnostic and fail the load).
\ The element type f32, extent token extent-n, and block-256 instantiate the
\ polymorphic operation signatures in lib/ptx/tile.f; the mask token threads from
\ GRID-CTX through LOAD / SCALE / +. / -. / /. to STORE by unification.

T-RESET

256 %BLOCK

TRUSTED: PTX-CHECK-REJECTS ( ptr u8 n -- )
   DIAGXT @ >r
   0 DIAGXT !
   CHECK! 0 T=
   r> DIAGXT ! ;

11 22 MK-SPAN 11 T=
11 12 22 MK-SPAN= 12 T= 11 T=
11 22 33 MK-MATRIX 11 T=

KERNEL: SAXPY ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-256
   {: x y a :}
   x GRID-CTX {: g :}
   x g LOAD  a SCALE
   y g LOAD  +.
   y g STORE ;

KERNEL: RELU-SPAN ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   dup GRID-CTX
   2dup LOAD RELU
   rot rot STORE ;

KERNEL: SUBDIV-SPAN ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: x y :} \ typed-local-lint: allow-bare-local
   x GRID-CTX {: g :} \ typed-local-lint: allow-bare-local
   x g LOAD  y g LOAD  -.
   y g LOAD  /.
   y g STORE ;

KERNEL: FMA-SPAN ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- )  GRID: ceil-n-256
   {: x y a :} \ typed-local-lint: allow-bare-local
   x GRID-CTX {: g :} \ typed-local-lint: allow-bare-local
   a x g LOAD  y g LOAD  FMA.
   y g STORE ;

s" PTX-GOOD-MASK-SHARED {: s :} s GRID-CTX {: g :} s g LOAD s g LOAD +." CHECK! -1 T=
s" PTX-BAD-MASK-DISTINCT {: s :} s GRID-CTX {: g1 :} s GRID-CTX {: g2 :} s g1 LOAD s g2 LOAD +." PTX-CHECK-REJECTS
s" PTX-GOOD-MASK-SUBDIV {: s :} s GRID-CTX {: g :} s g LOAD s g LOAD -. s g LOAD /." CHECK! -1 T= \ typed-local-lint: allow-bare-local
s" PTX-BAD-MASK-SUB {: s :} s GRID-CTX {: g1 :} s GRID-CTX {: g2 :} s g1 LOAD s g2 LOAD -." PTX-CHECK-REJECTS \ typed-local-lint: allow-bare-local
s" PTX-GOOD-FMA-MASK {: s a :} s GRID-CTX {: g :} a s g LOAD s g LOAD FMA." CHECK! -1 T= \ typed-local-lint: allow-bare-local
s" PTX-BAD-FMA-MASK {: s a :} s GRID-CTX {: g1 :} s GRID-CTX {: g2 :} a s g1 LOAD s g2 LOAD FMA." PTX-CHECK-REJECTS \ typed-local-lint: allow-bare-local
s" PTX-GOOD-MK-SPAN= ( ptr<space-global,f32> ptr<space-global,f32> u32 -- ) MK-SPAN= over GRID-CTX rot drop LOAD drop" CHECK! -1 T=
s" PTX-BAD-MK-SPAN-LONE ( ptr<space-global,f32> ptr<space-global,f32> u32 u32 -- ) {: p q n m :} p n MK-SPAN {: x :} q m MK-SPAN {: y :} x GRID-CTX y swap LOAD drop" PTX-CHECK-REJECTS \ typed-local-lint: allow-bare-local
s" PTX-BAD-SPACE ( span<space-shared,f32,extent-n> gridctx<block-256,extent-n,mask-live> -- tile<f32,block-256,mask-live> ) LOAD" PTX-CHECK-REJECTS
s" PTX-BAD-EXTENT ( span<space-global,f32,extent-r> gridctx<block-256,extent-c,mask-live> -- tile<f32,block-256,mask-live> ) LOAD" PTX-CHECK-REJECTS
s" PTX-BAD-TILE-MASK ( tile<f32,block-256,mask-a> tile<f32,block-256,mask-b> -- tile<f32,block-256,mask-a> ) +." PTX-CHECK-REJECTS
s" PTX-BAD-MISSING-CTX ( span<space-global,f32,extent-n> -- tile<f32,block-256,mask-live> ) LOAD" PTX-CHECK-REJECTS
s" PTX-BAD-ROWCTX-AS-GRID ( span<space-global,f32,extent-n> rowctx<block-256,extent-n,mask-live> -- tile<f32,block-256,mask-live> ) LOAD" PTX-CHECK-REJECTS
s" PTX-BAD-SPAN-ARITH ( span<space-global,f32,extent-n> n -- n ) +" PTX-CHECK-REJECTS
s" PTX-BAD-SPAN-SYNTAX ( span<space-global,f32,extent-n -- )" PTX-CHECK-REJECTS
SGBAD @ TTRUE

\ Clean load past this point is the positive proof: KERNEL: verified SAXPY's body
\ against its declared parametric effect. A reject emits a diagnostic + fails load.

T-REPORT
