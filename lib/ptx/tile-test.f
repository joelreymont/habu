\ ptx-tile-test.f - checked SAXPY kernel over the M4 tile vocabulary.
\
\ The KERNEL: definition IS the positive proof: it certifies against its declared
\ parametric effect (a reject would emit a checker diagnostic and fail the load).
\ The element type f32, extent token extent-n, and block-256 instantiate the
\ polymorphic operation signatures in lib/ptx/tile.f; the mask token threads from
\ GRID-CTX through LOAD / SCALE / +. / -. / /. to STORE by unification.

require lib/ptx/test-prelude.f
require test/checker-assert.f

T-RESET

256 %BLOCK

: PTX-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

11 22 MK-SPAN 11 T=
11 22 MK-SPAN-ONCE 11 T=
11 12 22 MK-SPAN= 12 T= 11 T=
11 22 33 MK-MATRIX 11 T=
11 22 33 MK-MATRIX-ONCE 11 T=

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

KERNEL: SCATTER-SPAN ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: x y :} \ typed-local-lint: allow-bare-local
   x GRID-CTX {: g :} \ typed-local-lint: allow-bare-local
   x g LOAD
   y g SCATTER-ADD ;

KERNEL: ONCE-SPAN ( span<space-global-once,f32,extent-n> -- )  GRID: ceil-n-256
   {: x :} \ typed-local-lint: allow-bare-local
   x GRID-CTX-ONCE {: g :} \ typed-local-lint: allow-bare-local
   x g LOAD-ONCE
   x g STORE-ONCE ;

KERNEL: FANIN-PTRS ( ptr<space-global,f32> ptr<space-global,f32> -- )  GRID: ceil-n-256
   {: x:a y:b :}
   x FANIN-CTX {: g:c :}
   x g FANIN-LOAD
   y g FANIN-SCATTER-ADD ;

KERNEL: INDEX-GATHER ( span<space-global,u32,extent-i> span<space-global,f32,extent-d> span<space-global,f32,extent-i> -- )  GRID: ceil-n-256
   {: idx:a data:b out:c :}
   idx data INDEX-CTX {: g:d :}
   idx data g INDEX-LOAD
   out g INDEX-DENSE-STORE ;

KERNEL: INDEX-SCATTER ( span<space-global,u32,extent-i> span<space-global,f32,extent-i> span<space-global,f32,extent-d> -- )  GRID: ceil-n-256
   {: idx:a vals:b out:c :}
   idx out INDEX-CTX {: g:d :}
   vals g INDEX-DENSE-LOAD
   idx out g INDEX-SCATTER-ADD ;

KERNEL: INDEX-UNIQUE ( span<space-global,u32,extent-i> span<space-global,f32,extent-i> span<space-global,f32,extent-d> -- )  GRID: ceil-n-256
   {: idx:a vals:b out:c :}
   idx out UNIQUE-INDEX-CTX {: g:d :}
   vals g UNIQUE-INDEX-DENSE-LOAD
   idx out g INDEX-STORE ;

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
s" PTX-GOOD-SCATTER-ADD {: s :} s GRID-CTX {: g :} s g LOAD s g SCATTER-ADD" CHECK! -1 T= \ typed-local-lint: allow-bare-local
s" PTX-BAD-SCATTER-ADD-MASK {: s :} s GRID-CTX {: g1 :} s GRID-CTX {: g2 :} s g1 LOAD s g2 SCATTER-ADD" PTX-CHECK-REJECTS \ typed-local-lint: allow-bare-local
s" PTX-GOOD-FANIN ( ptr<space-global,f32> ptr<space-global,f32> -- ) {: p:a q:b :} p FANIN-CTX {: g:c :} p g FANIN-LOAD q g FANIN-SCATTER-ADD" CHECK! -1 T=
s" PTX-BAD-FANIN-WITH-GRID ( ptr<space-global,f32> span<space-global,f32,extent-n> -- ) {: p:a s:b :} s GRID-CTX {: g:c :} p g FANIN-LOAD drop" PTX-CHECK-REJECTS
s" PTX-BAD-LOAD-WITH-FANIN ( ptr<space-global,f32> span<space-global,f32,extent-n> -- ) {: p:a s:b :} p FANIN-CTX {: g:c :} s g LOAD drop" PTX-CHECK-REJECTS
s" PTX-GOOD-INDEX ( span<space-global,u32,extent-i> span<space-global,f32,extent-d> -- ) {: ix:a xs:b :} ix xs INDEX-CTX {: g:c :} ix xs g INDEX-LOAD ix xs g INDEX-SCATTER-ADD" CHECK! -1 T=
s" PTX-GOOD-INDEX-DENSE ( span<space-global,u32,extent-i> span<space-global,f32,extent-d> span<space-global,f32,extent-i> -- ) {: ix:a xs:b ys:c :} ix xs INDEX-CTX {: g:d :} ix xs g INDEX-LOAD ys g INDEX-DENSE-STORE" CHECK! -1 T=
s" PTX-GOOD-INDEX-UNIQUE ( span<space-global,u32,extent-i> span<space-global,f32,extent-i> span<space-global,f32,extent-d> -- ) {: ix:a vals:b out:c :} ix out UNIQUE-INDEX-CTX {: g:d :} vals g UNIQUE-INDEX-DENSE-LOAD ix out g INDEX-STORE" CHECK! -1 T=
s" PTX-BAD-INDEX-WRONG-DATA ( span<space-global,u32,extent-i> span<space-global,f32,extent-d> span<space-global,f32,extent-e> -- ) {: ix:a xs:b ys:c :} ix xs INDEX-CTX {: g:d :} ix ys g INDEX-LOAD drop" PTX-CHECK-REJECTS
s" PTX-BAD-INDEX-WRONG-DENSE ( span<space-global,u32,extent-i> span<space-global,f32,extent-d> span<space-global,f32,extent-j> -- ) {: ix:a xs:b ys:c :} ix xs INDEX-CTX {: g:d :} ys g INDEX-DENSE-LOAD drop" PTX-CHECK-REJECTS
s" PTX-BAD-INDEX-STORE-NONUNIQUE ( tile<f32,block-256,mask-live> span<space-global,u32,extent-i> span<space-global,f32,extent-d> idxctx<block-256,extent-i,extent-d,mask-live> -- ) INDEX-STORE" PTX-CHECK-REJECTS
s" PTX-BAD-INDEX-SCATTER-UNIQUE ( tile<f32,block-256,mask-live> span<space-global,u32,extent-i> span<space-global,f32,extent-d> uniqidxctx<block-256,extent-i,extent-d,mask-live> -- ) INDEX-SCATTER-ADD" PTX-CHECK-REJECTS
s" PTX-GOOD-ONCE {: s :} s GRID-CTX-ONCE {: g :} s g LOAD-ONCE s g STORE-ONCE" CHECK! -1 T= \ typed-local-lint: allow-bare-local
s" PTX-BAD-ONCE-FROM-PLAIN ( tile<f32,block-256,mask-live> span<space-global,f32,extent-n> gridctx<block-256,extent-n,mask-live> -- ) STORE-ONCE" PTX-CHECK-REJECTS
s" PTX-BAD-PLAIN-FROM-ONCE ( tile<f32,block-256,mask-live> span<space-global-once,f32,extent-n> gridctx<block-256,extent-n,mask-live> -- ) STORE" PTX-CHECK-REJECTS
s" PTX-BAD-ONCE-CTX-FROM-PLAIN ( span<space-global,f32,extent-n> -- gridctx<block-256,extent-n,mask-live> ) GRID-CTX-ONCE" PTX-CHECK-REJECTS
s" PTX-GOOD-MK-SPAN= ( ptr<space-global,f32> ptr<space-global,f32> u32 -- ) MK-SPAN= over GRID-CTX rot drop LOAD drop" CHECK! -1 T=
s" PTX-BAD-MK-SPAN-LONE ( ptr<space-global,f32> ptr<space-global,f32> u32 u32 -- ) {: p q n m :} p n MK-SPAN {: x :} q m MK-SPAN {: y :} x GRID-CTX y swap LOAD drop" PTX-CHECK-REJECTS \ typed-local-lint: allow-bare-local
s" PTX-BAD-SPACE ( span<space-shared,f32,extent-n> gridctx<block-256,extent-n,mask-live> -- tile<f32,block-256,mask-live> ) LOAD" PTX-CHECK-REJECTS
s" PTX-BAD-STAGE-GRIDCTX ( span<space-global,f32,extent-n> gridctx<block-256,extent-n,mask-live> -- span<space-shared,f32,extent-n> ) STAGE" PTX-CHECK-REJECTS
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
