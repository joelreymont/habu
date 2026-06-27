\ ptx-cg.f - PTX codegen: emit-mode lowering for the tile ops.
\
\ The runtime value of a span/gridctx/tile/uniform (whose TYPE the checker tracks)
\ is a PTX REGISTER NUMBER; each op emits its instructions and returns a fresh
\ result register. Running a checked KERNEL: body in emit mode therefore produces
\ the kernel's PTX - so the SAME checked kernel that type-checks also emits, and
\ ptxas assembles it (proven for SAXPY on sm_87). Class is implied by the op
\ (span/ctx -> rd, tile/uniform -> f). Load after lib/errors.f, lib/string.f,
\ lib/fmt.f, and src/arch/ptx/emit.f (reuses PTX-L). Checked Habu.

variable CG-NF  variable CG-NRD  variable CG-NR  variable CG-NP

: CG-RESET ( -- )  2 CG-NF !  3 CG-NRD !  2 CG-NR !  1 CG-NP ! ;  \ after the param loads
: CG-NEXT-F  ( -- n )  CG-NF  @ dup 1+ CG-NF  ! ;
: CG-NEXT-RD ( -- n )  CG-NRD @ dup 1+ CG-NRD ! ;
: CG-NEXT-R  ( -- n )  CG-NR  @ dup 1+ CG-NR  ! ;
: CG-NEXT-P  ( -- n )  CG-NP  @ dup 1+ CG-NP  ! ;

\ append operands / literals to the shared string builder, then emit the line
: CG-S  ( ptr u8 n -- )  SB-APPEND ;
: CG-F  ( n -- )  s" %f"  SB-APPEND SB-U ;
: CG-RD ( n -- )  s" %rd" SB-APPEND SB-U ;
: CG-R  ( n -- )  s" %r"  SB-APPEND SB-U ;
: CG-P  ( n -- )  s" %p"  SB-APPEND SB-U ;
: CG-LINE ( -- )  SB$ PTX-L ;

\ --- module / entry scaffolding ---
: CG-HEADER ( -- )  PTX-HEADER-SM87  PTX-NL ;
: CG-ENTRY ( -- )
   s" .visible .entry SAXPY(.param .u64 p_x, .param .u64 p_y, .param .f32 p_a, .param .u32 p_n)" PTX-L ;
: CG-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<8>;" PTX-L
   s" .reg .f32 %f<32>;" PTX-L
   s" .reg .b32 %r<16>;" PTX-L
   s" .reg .b64 %rd<16>;" PTX-L ;
: CG-PARAMS ( -- )                      \ x=%rd1 y=%rd2 a=%f1 n=%r1
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_y];" PTX-L
   s" ld.param.f32 %f1, [p_a];" PTX-L
   s" ld.param.u32 %r1, [p_n];" PTX-L ;
: CG-RET ( -- )   s" DONE:" PTX-L  s" ret;" PTX-L ;
: CG-CLOSE ( -- ) s" }" PTX-L ;

\ --- per-op emitters (operate on register numbers) ---
\ GRID-CTX: global flat index + bounds predicate; returns the byte-offset rd reg.
: EMIT-GRID-CTX ( n -- n ) {: spanrd :}    \ span base unused (index is from tid)
   CG-NEXT-R {: rc :}  CG-NEXT-R {: rn :}  CG-NEXT-R {: rt :}  CG-NEXT-R {: ri :}
   SB-RESET s" mov.u32 " CG-S rc CG-R s" , %ctaid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rn CG-R s" , %ntid.x;"  CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;"   CG-S CG-LINE
   SB-RESET s" mad.lo.u32 " CG-S ri CG-R s" , " CG-S rc CG-R s" , " CG-S rn CG-R s" , " CG-S rt CG-R s" ;" CG-S CG-LINE
   CG-NEXT-P {: p :}
   SB-RESET s" setp.ge.u32 " CG-S p CG-P s" , " CG-S ri CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  bra DONE;" CG-S CG-LINE
   CG-NEXT-RD {: off :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S ri CG-R s" , 4;" CG-S CG-LINE
   off ;

\ LOAD: masked coalesced load from span base + ctx offset; returns tile f reg.
: EMIT-LOAD ( n n -- n ) {: spanrd ctxrd :}
   CG-NEXT-RD {: a :}
   SB-RESET s" cvta.to.global.u64 " CG-S a CG-RD s" , " CG-S spanrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S a CG-RD s" , " CG-S ctxrd CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-F {: t :}
   SB-RESET s" ld.global.f32 " CG-S t CG-F s" , [" CG-S a CG-RD s" ];" CG-S CG-LINE
   t ;

\ SCALE: tile * uniform -> tile (mul.rn, no contraction)
: EMIT-SCALE ( n n -- n ) {: tilef unif :}
   CG-NEXT-F {: r :}
   SB-RESET s" mul.rn.f32 " CG-S r CG-F s" , " CG-S unif CG-F s" , " CG-S tilef CG-F s" ;" CG-S CG-LINE
   r ;

\ +. : tile + tile -> tile (add.rn)
: EMIT-ADD ( n n -- n ) {: a b :}
   CG-NEXT-F {: r :}
   SB-RESET s" add.rn.f32 " CG-S r CG-F s" , " CG-S a CG-F s" , " CG-S b CG-F s" ;" CG-S CG-LINE
   r ;

\ STORE: tile -> span base + ctx offset (active lanes)
: EMIT-STORE ( n n n -- ) {: tilef spanrd ctxrd :}
   CG-NEXT-RD {: a :}
   SB-RESET s" cvta.to.global.u64 " CG-S a CG-RD s" , " CG-S spanrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S a CG-RD s" , " CG-S ctxrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" st.global.f32 [" CG-S a CG-RD s" ], " CG-S tilef CG-F s" ;" CG-S CG-LINE ;

\ --- SAXPY emit, lowering the checked dataflow x g LOAD a SCALE  y g LOAD +.  y g STORE ---
\ x=%rd1, y=%rd2, a=%f1 (set by CG-PARAMS).
: EMIT-SAXPY-BODY ( -- )
   1 EMIT-GRID-CTX {: g :}          \ g = byte-offset reg
   1 g EMIT-LOAD  1 EMIT-SCALE      \ x g LOAD a SCALE  -> tile
   2 g EMIT-LOAD  EMIT-ADD          \ y g LOAD +.       -> tile
   2 g EMIT-STORE ;                 \ y g STORE

: EMIT-SAXPY ( -- )
   CG-RESET
   CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   EMIT-SAXPY-BODY
   CG-RET CG-CLOSE ;
