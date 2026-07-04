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
   s" .reg .pred %p<32>;" PTX-L
   s" .reg .f32 %f<32>;" PTX-L
   s" .reg .b32 %r<32>;" PTX-L
   s" .reg .b64 %rd<32>;" PTX-L ;
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
TRUSTED: PTR-REG ( n -- ptr<space-global,f32> ) ;
TRUSTED: SPAN-ONCE-REG ( n -- span<space-global-once,f32,extent-n> ) ;
TRUSTED: INDEX-SPAN-REG ( n -- span<space-global,u32,extent-i> ) ;
TRUSTED: INDEX-VALUE-SPAN-REG ( n -- span<space-global,f32,extent-i> ) ;
TRUSTED: DATA-SPAN-REG ( n -- span<space-global,f32,extent-d> ) ;
TRUSTED: MATRIX-REG ( n -- matrix<space-global,f32,extent-r,extent-c> ) ;
TRUSTED: MATRIX-ONCE-REG ( n -- matrix<space-global-once,f32,extent-r,extent-c> ) ;

\ --- f64 -> f32 IEEE-754 marshalling (host side: kernel params/arrays are f32,
\ Habu floats are 64-bit cells). R>BITS reinterprets a float as its 64-bit pattern
\ (the one thin trusted cast). F64>F32 narrows to the 32-bit pattern with correct
\ round-to-nearest-even and every IEEE special: signed zero, gradual underflow
\ (f32 subnormals), overflow -> +/-inf, and NaN preserved as a quiet NaN (payload
\ kept). Normal values within f32 range are exact when representable. ---
TRUSTED: R>BITS ( r -- n ) ;

\ round a 53-bit significand right by sh bits (sh>=30) into a subnormal / min-normal
\ f32 magnitude, round-to-nearest-even, then OR the pre-shifted sign s. sh>53 is
\ below half a ULP of the smallest subnormal -> signed zero. A round-up that carries
\ out of 23 bits lands on 0x00800000, the smallest normal, which is correct.
: SUBN>F32 ( n n n -- n ) {: sig:n sh:n s:n :}
   sh 53 > if s exit then                            \ too small -> signed zero
   sig sh rshift {: kept:n :}
   sig sh 1 - rshift 1 and {: g:n :}                 \ round (guard) bit
   1 sh 1 - lshift 1 - sig and {: sticky:n :}        \ bits below the guard
   g 0= if s kept or exit then                       \ guard 0 -> round down
   sticky 0= 0= if s kept 1 + or exit then           \ guard 1 + sticky -> round up
   kept 1 and 0= if s kept or exit then              \ exact tie, even -> down
   s kept 1 + or ;                                   \ exact tie, odd -> up

: F64>F32 ( r -- n ) {: fr:r :}
   fr R>BITS {: b:n :}
   b 63 rshift 1 and 31 lshift {: s:n :}             \ sign, already at bit 31
   b 52 rshift $7FF and {: e:n :}                     \ f64 biased exponent
   b $FFFFFFFFFFFFF and {: m:n :}                     \ 52-bit mantissa
   e 896 - {: x:n :}                                  \ target f32 biased exponent
   e $7FF = if                                        \ inf / NaN
      m 0= if s $7F800000 or exit then                \ +/-inf
      s $7F800000 or  m 29 rshift or  $400000 or exit \ quiet NaN (payload kept)
   then
   e 0= if s exit then                                \ +/-0 / f64-subnormal -> signed 0
   x 1 < if  1 52 lshift m or  30 x -  s  SUBN>F32  exit  then   \ f32 subnormal
   x 254 > if s $7F800000 or exit then                \ overflow -> +/-inf
   m 29 rshift {: mt:n :}                             \ top 23 mantissa bits
   m $1FFFFFFF and {: rem:n :}                        \ the 29 dropped bits
   rem $10000000 > if 1 else
      rem $10000000 = if mt 1 and else 0 then
   then {: inc:n :}                                   \ round-to-nearest-even increment
   x 23 lshift mt or inc + {: v:n :}                  \ carry from mantissa bumps exponent
   v $7F7FFFFF > if s $7F800000 or exit then          \ carry overflowed to inf
   s v or ;

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

\ --- f32 array (de)packing: marshal a contiguous f64-cell buffer to/from a packed
\ little-endian f32 array for device upload/readback. SF-ST/SF-LD are the raw
\ 4-byte little-endian store/load; F32-PACK narrows each cell (F64>F32), F32-UNPACK
\ widens each word (F32>F64). ---
: SF-ST ( n ptr u8 -- ) {: v:n p:ptr :}            \ store low 32 bits of v LE at p
   v           $FF and  p     c!
   v 8 rshift  $FF and  p 1 + c!
   v 16 rshift $FF and  p 2 + c!
   v 24 rshift $FF and  p 3 + c! ;
: SF-LD ( ptr u8 -- n ) {: p:ptr :}                \ load a LE 32-bit word at p
   p     c@
   p 1 + c@ 8  lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;
: F32-PACK ( ptr a n ptr u8 -- ) {: src:ptr cnt:n dst:ptr :}   \ n f64 cells -> f32
   cnt 0 ?do  src i cells + @ F64>F32  dst i 4 * +  SF-ST  loop ;
: F32-UNPACK ( ptr u8 n ptr a -- ) {: src:ptr cnt:n dst:ptr :} \ f32 -> n f64 cells
   cnt 0 ?do  src i 4 * +  SF-LD  F32>F64  dst i cells + !  loop ;

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

: EMIT-GRID-CTX-ONCE ( n -- n )
   EMIT-GRID-CTX ;

: EMIT-COOP-CTX ( n -- n ) {: spanrd:n :}
   spanrd drop
   CG-NEXT-R {: rt:n :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   rt ;

: EMIT-LOAD-ONCE ( n n -- n )
   EMIT-LOAD ;

: EMIT-STAGE ( n n -- n ) {: spanrd:n ctxr:n :}
   CG-NEXT-RD {: off:n :}  CG-NEXT-RD {: g:n :}  CG-NEXT-RD {: a:n :}
   CG-NEXT-F {: t:n :}     CG-NEXT-R {: roff:n :} CG-NEXT-R {: sm:n :}  CG-NEXT-R {: sa:n :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S ctxr CG-R s" , 4;" CG-S CG-LINE
   SB-RESET s" cvta.to.global.u64 " CG-S g CG-RD s" , " CG-S spanrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S g CG-RD s" , " CG-S off CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" ld.global.f32 " CG-S t CG-F s" , [" CG-S a CG-RD s" ];" CG-S CG-LINE
   SB-RESET s" shl.b32 " CG-S roff CG-R s" , " CG-S ctxr CG-R s" , 2;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S sm CG-R s" , SMEM;" CG-S CG-LINE
   SB-RESET s" add.s32 " CG-S sa CG-R s" , " CG-S sm CG-R s" , " CG-S roff CG-R s" ;" CG-S CG-LINE
   SB-RESET s" st.shared.f32 [" CG-S sa CG-R s" ], " CG-S t CG-F s" ;" CG-S CG-LINE
   SB-RESET s" bar.sync 0;" CG-S CG-LINE
   sm ;

: EMIT-SLOAD ( n n -- n ) {: shrd:n ctxrd:n :}
   CG-NEXT-R {: roff:n :}  CG-NEXT-R {: a:n :}
   SB-RESET s" shl.b32 " CG-S roff CG-R s" , " CG-S ctxrd CG-R s" , 2;" CG-S CG-LINE
   SB-RESET s" add.s32 " CG-S a CG-R s" , " CG-S shrd CG-R s" , " CG-S roff CG-R s" ;" CG-S CG-LINE
   CG-NEXT-F {: r:n :}
   SB-RESET s" ld.shared.f32 " CG-S r CG-F s" , [" CG-S a CG-R s" ];" CG-S CG-LINE
   r ;

: EMIT-SSTORE ( n n n -- ) {: tilef:n shrd:n ctxrd:n :}
   CG-NEXT-R {: roff:n :}  CG-NEXT-R {: a:n :}
   SB-RESET s" shl.b32 " CG-S roff CG-R s" , " CG-S ctxrd CG-R s" , 2;" CG-S CG-LINE
   SB-RESET s" add.s32 " CG-S a CG-R s" , " CG-S shrd CG-R s" , " CG-S roff CG-R s" ;" CG-S CG-LINE
   SB-RESET s" st.shared.f32 [" CG-S a CG-R s" ], " CG-S tilef CG-F s" ;" CG-S CG-LINE
   SB-RESET s" bar.sync 0;" CG-S CG-LINE ;

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

: EMIT-STORE-ONCE ( n n n -- )
   EMIT-STORE ;

\ SCATTER-ADD: conservative LOAD adjoint; cotangents accumulate at span+ctx.
: EMIT-SCATTER-ADD ( n n n -- ) {: tilef:n spanrd:n ctxrd:n :}
   CG-NEXT-RD {: a:n :}
   SB-RESET s" cvta.to.global.u64 " CG-S a CG-RD s" , " CG-S spanrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S a CG-RD s" , " CG-S ctxrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" red.global.add.f32 [" CG-S a CG-RD s" ], " CG-S tilef CG-F s" ;" CG-S CG-LINE ;

\ FANIN-CTX: active lanes are bounded by %r1, but all lanes address the scalar
\ base pointer rather than span+lane offset.
: EMIT-FANIN-CTX ( n -- n ) {: ptrrd:n :}
   ptrrd drop
   CG-NEXT-R {: rc:n :}  CG-NEXT-R {: rn:n :}  CG-NEXT-R {: rt:n :}  CG-NEXT-R {: ri:n :}
   SB-RESET s" mov.u32 " CG-S rc CG-R s" , %ctaid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rn CG-R s" , %ntid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   SB-RESET s" mad.lo.u32 " CG-S ri CG-R s" , " CG-S rc CG-R s" , " CG-S rn CG-R s" , " CG-S rt CG-R s" ;" CG-S CG-LINE
   CG-NEXT-P {: p:n :}
   SB-RESET s" setp.ge.u32 " CG-S p CG-P s" , " CG-S ri CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  bra DONE;" CG-S CG-LINE
   0 ;

: EMIT-FANIN-LOAD ( n n -- n ) {: ptrrd:n ctx:n :}
   ctx drop
   CG-NEXT-RD {: a:n :}
   SB-RESET s" cvta.to.global.u64 " CG-S a CG-RD s" , " CG-S ptrrd CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-F {: t:n :}
   SB-RESET s" ld.global.f32 " CG-S t CG-F s" , [" CG-S a CG-RD s" ];" CG-S CG-LINE
   t ;

: EMIT-FANIN-SCATTER-ADD ( n n n -- ) {: tilef:n ptrrd:n ctx:n :}
   ctx drop
   CG-NEXT-RD {: a:n :}
   SB-RESET s" cvta.to.global.u64 " CG-S a CG-RD s" , " CG-S ptrrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" red.global.add.f32 [" CG-S a CG-RD s" ], " CG-S tilef CG-F s" ;" CG-S CG-LINE ;

\ INDEX-CTX: active lanes are bounded by %r1. Indexed operations use the lane
\ to load a u32 index, check it against %r2, then address data[index].
: EMIT-INDEX-CTX ( n n -- n ) {: idxrd:n datard:n :}
   idxrd drop
   datard drop
   CG-NEXT-R {: rc:n :}  CG-NEXT-R {: rn:n :}  CG-NEXT-R {: rt:n :}  CG-NEXT-R {: ri:n :}
   SB-RESET s" mov.u32 " CG-S rc CG-R s" , %ctaid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rn CG-R s" , %ntid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   SB-RESET s" mad.lo.u32 " CG-S ri CG-R s" , " CG-S rc CG-R s" , " CG-S rn CG-R s" , " CG-S rt CG-R s" ;" CG-S CG-LINE
   CG-NEXT-P {: p:n :}
   SB-RESET s" setp.ge.u32 " CG-S p CG-P s" , " CG-S ri CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  bra DONE;" CG-S CG-LINE
   ri ;

: EMIT-UNIQUE-INDEX-CTX ( n n -- n )
   EMIT-INDEX-CTX ;

: EMIT-INDEX-OFFSET ( n n -- n ) {: idxrd:n ctxr:n :}
   CG-NEXT-RD {: ib:n :}  CG-NEXT-RD {: io:n :}  CG-NEXT-RD {: ia:n :}
   CG-NEXT-R {: idx:n :}  CG-NEXT-P {: p:n :}    CG-NEXT-RD {: off:n :}
   SB-RESET s" cvta.to.global.u64 " CG-S ib CG-RD s" , " CG-S idxrd CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" mul.wide.u32 " CG-S io CG-RD s" , " CG-S ctxr CG-R s" , 4;" CG-S CG-LINE
   SB-RESET s" add.u64 " CG-S ia CG-RD s" , " CG-S ib CG-RD s" , " CG-S io CG-RD s" ;" CG-S CG-LINE
   SB-RESET s" ld.global.u32 " CG-S idx CG-R s" , [" CG-S ia CG-RD s" ];" CG-S CG-LINE
   SB-RESET s" setp.ge.u32 " CG-S p CG-P s" , " CG-S idx CG-R s" , %r2;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  bra DONE;" CG-S CG-LINE
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S idx CG-R s" , 4;" CG-S CG-LINE
   off ;

: EMIT-LANE-OFFSET ( n -- n ) {: ctxr:n :}
   CG-NEXT-RD {: off:n :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S ctxr CG-R s" , 4;" CG-S CG-LINE
   off ;

: EMIT-INDEX-DENSE-LOAD ( n n -- n ) {: spanrd:n ctxr:n :}
   spanrd ctxr EMIT-LANE-OFFSET EMIT-LOAD ;

: EMIT-UNIQUE-INDEX-DENSE-LOAD ( n n -- n )
   EMIT-INDEX-DENSE-LOAD ;

: EMIT-INDEX-DENSE-STORE ( n n n -- ) {: tilef:n spanrd:n ctxr:n :}
   tilef spanrd ctxr EMIT-LANE-OFFSET EMIT-STORE ;

: EMIT-INDEX-LOAD ( n n n -- n ) {: idxrd:n spanrd:n ctxr:n :}
   spanrd idxrd ctxr EMIT-INDEX-OFFSET EMIT-LOAD ;

: EMIT-INDEX-SCATTER-ADD ( n n n n -- ) {: tilef:n idxrd:n spanrd:n ctxr:n :}
   tilef spanrd idxrd ctxr EMIT-INDEX-OFFSET EMIT-SCATTER-ADD ;

: EMIT-INDEX-STORE ( n n n n -- ) {: tilef:n idxrd:n spanrd:n ctxr:n :}
   tilef spanrd idxrd ctxr EMIT-INDEX-OFFSET EMIT-STORE ;

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
