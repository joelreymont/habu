\ cg-vec.f - PTX codegen: VECTORIZED (v4) emit-mode lowering for the tile ops.
\
\ The scalar path (cg.f) emits one ld.global.f32 per thread (1 element/thread).
\ This emits ld.global.v4.f32 / st.global.v4.f32 (4 contiguous floats per
\ instruction) with each thread owning 4 consecutive elements - the codegen lever
\ that closes the Habu-vs-Triton bandwidth gap (Triton uses v2; v4 is wider).
\
\ Representation: a v4 tile is the BASE of a group of 4 consecutive %f registers
\ (%f<base> .. %f<base+3>), one per lane. The CHECKER still sees a plain
\ tile<t,b,m> (tile-v4.f) - vectorization is purely a codegen detail, no type
\ change. PRECONDITION: element count n % 4 == 0 (the bench + device goldens
\ satisfy it; the masked scalar-residual tail for general n is dot
\ habu-scalar-residual-tail-...). Load after lib/ptx/cg.f. Checked Habu.

: CG-NEXT-F4 ( -- n )  CG-NF @ dup 4 + CG-NF ! ;   \ 4 consecutive lane regs

\ {%f<b>, %f<b+1>, %f<b+2>, %f<b+3>}  - the v4 register vector operand
: CG-F4 ( n -- ) {: b :}
   s" {" CG-S  b CG-F  s" , " CG-S  b 1+ CG-F  s" , " CG-S
   b 2 + CG-F  s" , " CG-S  b 3 + CG-F  s" }" CG-S ;

\ GRID-CTX-V4: each thread owns 4 consecutive elements [4*idx .. 4*idx+3].
\ base_elem = idx*4; bounds base_elem >= n -> DONE (safe when n%4==0); the byte
\ offset is base_elem*4 = idx*16. Returns the byte-offset rd reg.
: EMIT-GRID-CTX-V4 ( n -- n ) {: spanrd :}
   CG-NEXT-R {: rc :}  CG-NEXT-R {: rn :}  CG-NEXT-R {: rt :}
   CG-NEXT-R {: ri :}  CG-NEXT-R {: be :}
   SB-RESET s" mov.u32 " CG-S rc CG-R s" , %ctaid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rn CG-R s" , %ntid.x;"  CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;"   CG-S CG-LINE
   SB-RESET s" mad.lo.u32 " CG-S ri CG-R s" , " CG-S rc CG-R s" , " CG-S rn CG-R s" , " CG-S rt CG-R s" ;" CG-S CG-LINE
   SB-RESET s" shl.b32 " CG-S be CG-R s" , " CG-S ri CG-R s" , 2;" CG-S CG-LINE   \ be = idx*4
   CG-NEXT-P {: p :}
   SB-RESET s" setp.ge.u32 " CG-S p CG-P s" , " CG-S be CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  bra DONE;" CG-S CG-LINE
   CG-NEXT-RD {: off :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S ri CG-R s" , 16;" CG-S CG-LINE   \ bytes = idx*16
   off ;

\ effective global address = cvta.to.global(span) + ctx byte offset
: CG-VEC-ADDR ( n n -- n ) {: spanrd ctxrd :}
   CG-NEXT-RD {: a :}
   SB-RESET s" cvta.to.global.u64 " CG-S a CG-RD s" , " CG-S spanrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S a CG-RD s" , " CG-S ctxrd CG-RD s" ;" CG-S CG-LINE
   a ;

\ LOAD-V4: one 16-byte vector load into 4 lane regs; returns the tile base reg.
: EMIT-LOAD-V4 ( n n -- n ) {: spanrd ctxrd :}
   spanrd ctxrd CG-VEC-ADDR {: a :}
   CG-NEXT-F4 {: t :}
   SB-RESET s" ld.global.v4.f32 " CG-S t CG-F4 s" , [" CG-S a CG-RD s" ];" CG-S CG-LINE
   t ;

\ SCALE-V4: lane-wise tile * uniform (broadcast scalar) -> tile
: EMIT-SCALE-V4 ( n n -- n ) {: tb unif :}
   CG-NEXT-F4 {: r :}
   4 0 do
      SB-RESET s" mul.rn.f32 " CG-S  r i + CG-F  s" , " CG-S  unif CG-F  s" , " CG-S  tb i + CG-F  s" ;" CG-S CG-LINE
   loop
   r ;

\ ADD-V4: lane-wise tile + tile -> tile
: EMIT-ADD-V4 ( n n -- n ) {: a b :}
   CG-NEXT-F4 {: r :}
   4 0 do
      SB-RESET s" add.rn.f32 " CG-S  r i + CG-F  s" , " CG-S  a i + CG-F  s" , " CG-S  b i + CG-F  s" ;" CG-S CG-LINE
   loop
   r ;

\ RELU-V4: lane-wise max(tile, 0) -> tile
: EMIT-RELU-V4 ( n -- n ) {: tb :}
   CG-NEXT-F4 {: r :}
   4 0 do
      SB-RESET s" max.f32 " CG-S  r i + CG-F  s" , " CG-S  tb i + CG-F  s" , 0f00000000;" CG-S CG-LINE
   loop
   r ;

\ STORE-V4: one 16-byte vector store of the 4 lane regs
: EMIT-STORE-V4 ( n n n -- ) {: tb spanrd ctxrd :}
   spanrd ctxrd CG-VEC-ADDR {: a :}
   SB-RESET s" st.global.v4.f32 [" CG-S a CG-RD s" ], " CG-S tb CG-F4 s" ;" CG-S CG-LINE ;
