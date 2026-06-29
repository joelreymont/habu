\ ptx-cg.f - PTX codegen: emit-mode lowering for the tile ops.
\
\ The runtime value of a span/gridctx/tile/uniform (whose TYPE the checker tracks)
\ is a PTX REGISTER NUMBER; each op emits its instructions and returns a fresh
\ result register. Running a checked KERNEL: body in emit mode therefore produces
\ the kernel's PTX - so the SAME checked kernel that type-checks also emits, and
\ ptxas assembles it (proven for SAXPY on sm_87). Class is implied by the op
\ (span/ctx -> rd, tile/uniform -> f). Load after lib/errors.f, lib/string.f,
\ lib/fmt.f, and src/arch/ptx/emit.f (reuses PTX-L). Checked Habu.

variable CG-NF  variable CG-NRD  variable CG-NR  variable CG-NP  variable CG-NL

0 constant CG-OP-ADD
1 constant CG-OP-SUB
2 constant CG-OP-MUL
3 constant CG-OP-DIV

: CG-RESET ( -- )  2 CG-NF !  3 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;  \ after param loads
: CG-NEXT-F  ( -- n )  CG-NF  @ dup 1+ CG-NF  ! ;
: CG-NEXT-RD ( -- n )  CG-NRD @ dup 1+ CG-NRD ! ;
: CG-NEXT-R  ( -- n )  CG-NR  @ dup 1+ CG-NR  ! ;
: CG-NEXT-P  ( -- n )  CG-NP  @ dup 1+ CG-NP  ! ;
: CG-NEXT-L  ( -- n )  CG-NL  @ dup 1+ CG-NL  ! ;   \ fresh PTX label id

\ append operands / literals to the shared string builder, then emit the line
: CG-S  ( ptr u8 n -- )  SB-APPEND ;
: CG-F  ( n -- )  s" %f"  SB-APPEND SB-U ;
: CG-RD ( n -- )  s" %rd" SB-APPEND SB-U ;
: CG-R  ( n -- )  s" %r"  SB-APPEND SB-U ;
: CG-P  ( n -- )  s" %p"  SB-APPEND SB-U ;
: CG-L  ( n -- )  s" $L"  SB-APPEND SB-U ;          \ label operand: $L<n>
: CG-LINE ( -- )  SB$ PTX-L ;
: CG-LDEF ( n -- )
   SB-RESET CG-L s" :" CG-S CG-LINE ;

: CG-BIN-OP$ ( n -- ptr u8 n )
   case
      CG-OP-ADD of s" add.rn.f32 " endof
      CG-OP-SUB of s" sub.rn.f32 " endof
      CG-OP-MUL of s" mul.rn.f32 " endof
      CG-OP-DIV of s" div.rn.f32 " endof
      drop s" cg: unknown binary op" 76 die
   endcase ;

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

\ A kernel arg is a PTX register number; SPAN-REG / UNIFORM-REG assert its kernel
\ TYPE so the emit driver runs the checked kernel checked (the only unchecked
\ surface is these thin from-register identity casts - the codegen analogue of
\ MK-SPAN's from_raw_parts boundary). CG-PARAMS sets x=%rd1, y=%rd2, a=%f1.
TRUSTED: SPAN-REG ( n -- span<space-global,f32,extent-n> ) ;
TRUSTED: UNIFORM-REG ( n -- uniform<f32> ) ;

\ --- f64 -> f32 IEEE-754 marshalling (host side: kernel params/arrays are f32,
\ Habu floats are 64-bit cells). R>BITS reinterprets a float as its 64-bit pattern
\ (the one thin trusted cast). F64>F32 repacks to 32-bit (truncating mantissa,
\ flush-to-zero on under/overflow; normal range is exact; denormals/NaN are a
\ documented boundary). ---
TRUSTED: R>BITS ( r -- n ) ;

: F64>F32 ( r -- n ) {: r :}
   r R>BITS {: b :}
   b 63 rshift 1 and {: sgn :}
   b 52 rshift $7FF and {: e64 :}
   b $FFFFFFFFFFFFF and 29 rshift {: m32 :}        \ top 23 mantissa bits
   e64 896 - {: e32 :}                              \ rebias 1023 -> 127 (bound before control flow)
   e64 0= if 0 exit then                            \ +/-0 and flushed denormals
   e32 1 < if 0 exit then                           \ underflow -> 0
   e32 254 > if  sgn 31 lshift $7F800000 or  exit then   \ overflow -> +/-inf
   sgn 31 lshift  e32 23 lshift or  m32 or ;

\ inverse: read a device-returned f32 bit pattern back into a Habu f64 float (the
\ readback marshalling; BITS>R is the one thin trusted reinterpret). +/-0 and inf
\ are handled; f32 normals widen exactly, NaN is a documented boundary.
TRUSTED: BITS>R ( n -- r ) ;

: F32>F64 ( n -- r ) {: b :}
   b 31 rshift 1 and {: sgn :}
   b 23 rshift $FF and {: e32 :}
   b $7FFFFF and {: m32 :}                          \ 23 mantissa bits
   sgn 63 lshift {: hi :}                           \ sign in f64 bit 63
   e32 0= if hi BITS>R exit then                    \ +/-0 (denormals flush to 0)
   e32 $FF = if hi $7FF 52 lshift or BITS>R exit then   \ +/-inf
   hi  e32 896 + 52 lshift or  m32 29 lshift or  BITS>R ;

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

: EMIT-FMA ( n n n -- n ) {: unif:n tilef:n addf:n :}
   CG-NEXT-F {: r:n :}
   SB-RESET s" fma.rn.f32 " CG-S r CG-F
   s" , " CG-S unif CG-F s" , " CG-S tilef CG-F s" , " CG-S addf CG-F s" ;" CG-S CG-LINE
   r ;

: EMIT-BIN-F32 ( n n n -- n ) {: a:n b:n op:n :}
   CG-NEXT-F {: r:n :}
   SB-RESET op CG-BIN-OP$ CG-S r CG-F
   s" , " CG-S a CG-F s" , " CG-S b CG-F s" ;" CG-S CG-LINE
   r ;

\ +. : tile + tile -> tile (add.rn)
: EMIT-ADD ( n n -- n ) CG-OP-ADD EMIT-BIN-F32 ;

\ -. : tile - tile -> tile (sub.rn)
: EMIT-SUB ( n n -- n ) CG-OP-SUB EMIT-BIN-F32 ;

\ *. : tile * tile -> tile (mul.rn)
: EMIT-MUL ( n n -- n ) CG-OP-MUL EMIT-BIN-F32 ;

\ /. : tile / tile -> tile (div.rn)
: EMIT-DIV ( n n -- n ) CG-OP-DIV EMIT-BIN-F32 ;

\ RELU: max(tile, 0) -> tile (the elementwise nonlinearity; for fusion demos)
: EMIT-RELU ( n -- n ) {: tilef :}
   CG-NEXT-F {: r :}
   SB-RESET s" max.f32 " CG-S r CG-F s" , " CG-S tilef CG-F s" , 0f00000000;" CG-S CG-LINE
   r ;

\ STORE: tile -> span base + ctx offset (active lanes)
: EMIT-STORE ( n n n -- ) {: tilef spanrd ctxrd :}
   CG-NEXT-RD {: a :}
   SB-RESET s" cvta.to.global.u64 " CG-S a CG-RD s" , " CG-S spanrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S a CG-RD s" , " CG-S ctxrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" st.global.f32 [" CG-S a CG-RD s" ], " CG-S tilef CG-F s" ;" CG-S CG-LINE ;

\ ACC-ZERO: a fresh register accumulator = 0 (the gridctx is type-only here)
: EMIT-ACC-ZERO ( n -- n ) {: ctxrd :}
   CG-NEXT-F {: r :}
   SB-RESET s" mov.f32 " CG-S r CG-F s" , 0f00000000;" CG-S CG-LINE
   r ;

\ ACC-FMA: acc = a*b + acc  (one fused multiply-add K-step)
: EMIT-ACC-FMA ( n n n -- n ) {: accf af bf :}
   CG-NEXT-F {: r :}
   SB-RESET s" fma.rn.f32 " CG-S r CG-F s" , " CG-S af CG-F s" , " CG-S bf CG-F s" , " CG-S accf CG-F s" ;" CG-S CG-LINE
   r ;

\ ACC-TILE: finalize - the accumulator register IS the result tile (identity)
: EMIT-ACC-TILE ( n -- n ) ;

\ The per-op emitters above are what the CHECKED tile ops (lib/ptx/tile.f) call in
\ their TRUSTED: bodies, so RUNNING a checked KERNEL: body in emit mode produces
\ its PTX. The entry scaffolding (CG-HEADER..CG-PARAMS / CG-RET / CG-CLOSE) wraps
\ that body; see tools/ptx/saxpy-cg.f for the SAXPY driver. Param registers set by
\ CG-PARAMS: x=%rd1, y=%rd2, a=%f1, n=%r1.
