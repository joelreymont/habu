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
\ change. Full vectors use ld/st.global.v4.f32; partial residual vectors use
\ predicated scalar lanes, so general n is correct. Load after lib/ptx/cg.f.
\ Checked Habu.

: CG-NEXT-F4 ( -- n )  CG-NF @ dup 4 + CG-NF ! ;   \ 4 consecutive lane regs

\ {%f<b>, %f<b+1>, %f<b+2>, %f<b+3>}  - the v4 register vector operand
: CG-F4 ( n -- ) {: b:n :}
   s" {" CG-S  b CG-F  s" , " CG-S  b 1+ CG-F  s" , " CG-S
   b 2 + CG-F  s" , " CG-S  b 3 + CG-F  s" }" CG-S ;

\ GRID-CTX-V4: each thread owns 4 consecutive elements [4*idx .. 4*idx+3].
\ base_elem = idx*4; bounds base_elem >= n -> DONE. Returns the base element
\ index register; load/store decide vector-fast vs scalar-tail.
: EMIT-GRID-CTX-V4 ( n -- n ) {: spanrd:n :}
   CG-NEXT-R {: rc:n :}  CG-NEXT-R {: rn:n :}  CG-NEXT-R {: rt:n :}
   CG-NEXT-R {: ri:n :}  CG-NEXT-R {: be:n :}
   SB-RESET s" mov.u32 " CG-S rc CG-R s" , %ctaid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rn CG-R s" , %ntid.x;"  CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;"   CG-S CG-LINE
   SB-RESET s" mad.lo.u32 " CG-S ri CG-R s" , " CG-S rc CG-R s" , " CG-S rn CG-R s" , " CG-S rt CG-R s" ;" CG-S CG-LINE
   SB-RESET s" shl.b32 " CG-S be CG-R s" , " CG-S ri CG-R s" , 2;" CG-S CG-LINE   \ be = idx*4
   CG-NEXT-P {: p:n :}
   SB-RESET s" setp.ge.u32 " CG-S p CG-P s" , " CG-S be CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  bra DONE;" CG-S CG-LINE
   be ;

\ effective global address = cvta.to.global(span) + base_elem*4
: CG-VEC-ADDR ( n n -- n ) {: spanrd:n ctxrd:n :}
   CG-NEXT-RD {: off:n :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S ctxrd CG-R s" , 4;" CG-S CG-LINE
   CG-NEXT-RD {: a:n :}
   SB-RESET s" cvta.to.global.u64 " CG-S a CG-RD s" , " CG-S spanrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S a CG-RD s" , " CG-S off CG-RD s" ;" CG-S CG-LINE
   a ;

: CG-VEC-LANE-ADDR ( n n -- n ) {: base:n lane:n :}
   lane 0= if base exit then
   CG-NEXT-RD {: a:n :}
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S base CG-RD s" , " CG-S lane 4 * SB-U s" ;" CG-S CG-LINE
   a ;

: CG-VEC-LANE-INDEX ( n n -- n ) {: ctx:n lane:n :}
   lane 0= if ctx exit then
   CG-NEXT-R {: idx:n :}
   SB-RESET s" add.u32 " CG-S idx CG-R s" , " CG-S ctx CG-R s" , " CG-S lane SB-U s" ;" CG-S CG-LINE
   idx ;

: CG-VEC-TAIL-BRANCH ( n -- n n ) {: ctxrd:n :}
   CG-NEXT-L {: tail:n :}  CG-NEXT-L {: done:n :}
   CG-NEXT-R {: last:n :}  CG-NEXT-P {: ptail:n :}
   SB-RESET s" add.u32 " CG-S last CG-R s" , " CG-S ctxrd CG-R s" , 3;" CG-S CG-LINE
   SB-RESET s" setp.ge.u32 " CG-S ptail CG-P s" , " CG-S last CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S ptail CG-P s"  bra " CG-S tail CG-L s" ;" CG-S CG-LINE
   tail done ;

: EMIT-LOAD-LANE-V4 ( n n n n -- ) {: a:n ctxrd:n t:n lane:n :}
   ctxrd lane CG-VEC-LANE-INDEX {: idx:n :}
   CG-NEXT-P {: p:n :}
   SB-RESET s" setp.lt.u32 " CG-S p CG-P s" , " CG-S idx CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" mov.f32 " CG-S t lane + CG-F s" , 0f00000000;" CG-S CG-LINE
   a lane CG-VEC-LANE-ADDR {: la:n :}
   SB-RESET s" @" CG-S p CG-P s"  ld.global.f32 " CG-S t lane + CG-F s" , [" CG-S la CG-RD s" ];" CG-S CG-LINE ;

: EMIT-STORE-LANE-V4 ( n n n n -- ) {: a:n ctxrd:n t:n lane:n :}
   ctxrd lane CG-VEC-LANE-INDEX {: idx:n :}
   CG-NEXT-P {: p:n :}
   SB-RESET s" setp.lt.u32 " CG-S p CG-P s" , " CG-S idx CG-R s" , %r1;" CG-S CG-LINE
   a lane CG-VEC-LANE-ADDR {: la:n :}
   SB-RESET s" @" CG-S p CG-P s"  st.global.f32 [" CG-S la CG-RD s" ], " CG-S t lane + CG-F s" ;" CG-S CG-LINE ;

\ LOAD-V4: full vectors use one 16-byte vector load; residual vectors load only
\ active scalar lanes and seed inactive lane registers with 0.
: EMIT-LOAD-V4 ( n n -- n ) {: spanrd:n ctxrd:n :}
   spanrd ctxrd CG-VEC-ADDR {: a:n :}
   CG-NEXT-F4 {: t:n :}
   ctxrd CG-VEC-TAIL-BRANCH {: tail:n done:n :}
   SB-RESET s" ld.global.v4.f32 " CG-S t CG-F4 s" , [" CG-S a CG-RD s" ];" CG-S CG-LINE
   SB-RESET s" bra " CG-S done CG-L s" ;" CG-S CG-LINE
   tail CG-LDEF
   4 0 do
      a ctxrd t i EMIT-LOAD-LANE-V4
   loop
   done CG-LDEF
   t ;

\ SCALE-V4: lane-wise tile * uniform (broadcast scalar) -> tile
: EMIT-SCALE-V4 ( n n -- n ) {: tb:n unif:n :}
   CG-NEXT-F4 {: r:n :}
   4 0 do
      SB-RESET s" mul.rn.f32 " CG-S  r i + CG-F  s" , " CG-S  unif CG-F  s" , " CG-S  tb i + CG-F  s" ;" CG-S CG-LINE
   loop
   r ;

\ binary tile op: lane-wise tile op tile -> tile
: EMIT-BIN-F32-V4 ( n n n -- n ) {: a:n b:n op:n :}
   CG-NEXT-F4 {: r:n :}
   4 0 do
      SB-RESET op CG-BIN-OP$ CG-S  r i + CG-F
      s" , " CG-S  a i + CG-F  s" , " CG-S  b i + CG-F  s" ;" CG-S CG-LINE
   loop
   r ;

\ ADD-V4: lane-wise tile + tile -> tile
: EMIT-ADD-V4 ( n n -- n ) CG-OP-ADD EMIT-BIN-F32-V4 ;

\ SUB-V4: lane-wise tile - tile -> tile
: EMIT-SUB-V4 ( n n -- n ) CG-OP-SUB EMIT-BIN-F32-V4 ;

\ MUL-V4: lane-wise tile * tile -> tile
: EMIT-MUL-V4 ( n n -- n ) CG-OP-MUL EMIT-BIN-F32-V4 ;

\ DIV-V4: lane-wise tile / tile -> tile
: EMIT-DIV-V4 ( n n -- n ) CG-OP-DIV EMIT-BIN-F32-V4 ;

\ RELU-V4: lane-wise max(tile, 0) -> tile
: EMIT-RELU-V4 ( n -- n ) {: tb:n :}
   CG-NEXT-F4 {: r:n :}
   4 0 do
      SB-RESET s" max.f32 " CG-S  r i + CG-F  s" , " CG-S  tb i + CG-F  s" , 0f00000000;" CG-S CG-LINE
   loop
   r ;

\ STORE-V4: full vectors use one 16-byte vector store; residual vectors store
\ only active scalar lanes.
: EMIT-STORE-V4 ( n n n -- ) {: tb:n spanrd:n ctxrd:n :}
   spanrd ctxrd CG-VEC-ADDR {: a:n :}
   ctxrd CG-VEC-TAIL-BRANCH {: tail:n done:n :}
   SB-RESET s" st.global.v4.f32 [" CG-S a CG-RD s" ], " CG-S tb CG-F4 s" ;" CG-S CG-LINE
   SB-RESET s" bra " CG-S done CG-L s" ;" CG-S CG-LINE
   tail CG-LDEF
   4 0 do
      a ctxrd tb i EMIT-STORE-LANE-V4
   loop
   done CG-LDEF ;
