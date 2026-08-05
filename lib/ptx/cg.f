\ ptx-cg.f - PTX codegen: emit-mode lowering for the tile ops.
\
\ The runtime value of a span/gridctx/tile/uniform (whose TYPE the checker tracks)
\ is a PTX REGISTER NUMBER; each op emits its instructions and returns a fresh
\ result register. Running a checked KERNEL: body in emit mode therefore produces
\ the kernel's PTX - so the SAME checked kernel that type-checks also emits, and
\ ptxas assembles it (proven for SAXPY on sm_87). Class is implied by the op
\ (span/ctx -> rd, tile/uniform -> f). Checked Habu; dependencies are direct.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/kernel-abi.f
require lib/ieee754.f

\ Default kernel-ABI record: the SAXPY shape this scaffolding historically
\ hardcoded. The relu/exp/acc/... producers reuse the same entry name and
\ param layout on purpose (shared launcher; see tools/ptx/relu-cg.f), so the
\ default record IS their ABI. A driver with a different ABI declares its own
\ record (KABI:RESET KABI:NAME! KABI:SPAN+ ...) before CG-RESET/CG-ENTRY.
KABI:RESET
s" SAXPY" KABI:NAME!
s" ceil-n-256" KABI:GRID!
256 KABI:BLOCK!
s" x" s" n" KABI:SPAN+
s" y" s" n" KABI:SPAN+
s" a" KABI:UNIFORM+

variable CG-NF  variable CG-NRD  variable CG-NR  variable CG-NP  variable CG-NL
variable CG-GRID-IDX   \ flat grid index register last emitted by EMIT-GRID-CTX (for index-remap folds)

0 constant CG-OP-ADD
1 constant CG-OP-SUB
2 constant CG-OP-MUL
3 constant CG-OP-DIV

\ register seeds continue after the record's param loads (default record:
\ %rd1-2 + %f1 + %r1 -> seeds 3/2/2, the historical constants)
: CG-RESET ( -- )
   KABI:U64-N 1+ CG-NRD !
   KABI:F32-N 1+ CG-NF !
   KABI:U32-N 1+ CG-NR !
   1 CG-NP !  0 CG-NL ! ;
: CG-NEXT-F  ( -- n )  CG-NF  @ dup 1+ CG-NF  ! ;
: CG-NEXT-RD ( -- n )  CG-NRD @ dup 1+ CG-NRD ! ;
: CG-NEXT-R  ( -- n )  CG-NR  @ dup 1+ CG-NR  ! ;
: CG-NEXT-P  ( -- n )  CG-NP  @ dup 1+ CG-NP  ! ;
: CG-NEXT-L  ( -- n )  CG-NL  @ dup 1+ CG-NL  ! ;   \ fresh PTX label id

\ append operands / literals to the shared string builder, then emit the line
: CG-S  ( ptr u8 n -- )  SB-APPEND ;
: CG-F  ( n -- )  s" %f"  SB-APPEND FMT:SB-U ;
: CG-RD ( n -- )  s" %rd" SB-APPEND FMT:SB-U ;
: CG-R  ( n -- )  s" %r"  SB-APPEND FMT:SB-U ;
: CG-P  ( n -- )  s" %p"  SB-APPEND FMT:SB-U ;
: CG-L  ( n -- )  s" $L"  SB-APPEND FMT:SB-U ;          \ label operand: $L<n>
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
: CG-HEADER ( -- )  PTX-HEADER  PTX-NL ;

\ A PTX MODULE is exactly ONE .version/.target/.address_size header followed by N
\ `.visible .entry` kernels; ptxas rejects a second header in one stream. PTX-MODULE{
\ emits that header once, then each kernel emitted before }PTX-MODULE must append only
\ its entry+body (NO header of its own). A lone kernel that inlines CG-HEADER is the
\ degenerate N=1 module and stays valid, so single-kernel emitters are unchanged.
: PTX-MODULE{ ( -- )  CG-HEADER ;
: }PTX-MODULE ( -- ) ;                 \ documented module terminator (PTX has no footer)

\ entry + param loads render FROM the kernel-ABI record: one declaration
\ drives the .visible .entry line, the ld.param loads, the CG-RESET register
\ seeds, and the launch packing offsets (tools/ptx/cuda-launch.f).
: CG-ENTRY ( -- )
   KABI:ENTRY$ PTX-L ;
: CG-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<32>;" PTX-L
   s" .reg .f32 %f<32>;" PTX-L
   s" .reg .b32 %r<32>;" PTX-L
   s" .reg .b64 %rd<32>;" PTX-L ;
: CG-PARAMS ( -- )                      \ default record: x=%rd1 y=%rd2 a=%f1 n=%r1
   KABI:N-FIELDS 0 ?do
      i KABI:FIELD-PARAM? if i KABI:LD-LINE$ PTX-L then
   loop ;
: CG-RET ( -- )   s" DONE:" PTX-L  s" ret;" PTX-L ;
: CG-CLOSE ( -- ) s" }" PTX-L ;

\ A kernel arg is a PTX register number; SPAN-REG / UNIFORM-REG assert its kernel
\ TYPE so the emit driver runs the checked kernel checked (the only unchecked
\ surface is these thin from-register identity casts - the codegen analogue of
\ MK-SPAN's from_raw_parts boundary). CG-PARAMS sets x=%rd1, y=%rd2, a=%f1.
\ Retirement owner: habu-ptx-phantom-preserving-3df9db92.
TRUSTED: SPAN-REG ( n -- span<space-global,f32,extent-n> ) ;
TRUSTED: UNIFORM-REG ( n -- uniform<f32> ) ;
TRUSTED: PTR-REG ( n -- ptr<space-global,f32> ) ;
TRUSTED: SPAN-ONCE-REG ( n -- span<space-global-once,f32,extent-n> ) ;
TRUSTED: INDEX-SPAN-REG ( n -- span<space-global,u32,extent-i> ) ;
TRUSTED: INDEX-VALUE-SPAN-REG ( n -- span<space-global,f32,extent-i> ) ;
TRUSTED: DATA-SPAN-REG ( n -- span<space-global,f32,extent-d> ) ;
TRUSTED: MATRIX-REG ( n -- matrix<space-global,f32,extent-r,extent-c> ) ;
TRUSTED: MATRIX-ONCE-REG ( n -- matrix<space-global-once,f32,extent-r,extent-c> ) ;

\ --- f16 (IEEE half) narrowing for the fp16 mma tile (dot habu-fp16-mma-tile). F64>F16 mirrors
\ F32:NARROW (round-to-nearest-even; +/-0, subnormal, overflow->inf, NaN handled); the subnormal
\ rounding reuses IEEE754:ROUND-SHIFT-EVEN (it rounds the 53-bit f64 significand right by an
\ arbitrary shift and applies the sign, independent of the target width). f16 fields: sign bit15,
\ 5-bit exp (bias 15), 10-bit mantissa. SF-ST16 stores the low 16 bits little-endian; F16-PACK
\ narrows each f64 host cell to a packed f16 device buffer. Device C stays f32 (F32-BUF:UNPACK reads it
\ back), so no F16-UNPACK is needed. ---
: F64>F16 ( r -- n ) {: fr:r :}
   fr IEEE754:F64>BITS {: b:n :}
   b 63 rshift 1 and 15 lshift {: s :}               \ sign in f16 bit 15
   b 52 rshift $7FF and {: e :}                       \ f64 biased exponent
   b $FFFFFFFFFFFFF and {: m :}                        \ 52-bit mantissa
   e 1008 - {: x :}                                    \ target f16 biased exponent
   e $7FF = if                                         \ inf / NaN
      m 0= if s $7C00 or exit then                     \ +/-inf
      s $7C00 or  m 42 rshift or  $200 or exit         \ quiet NaN (payload kept)
   then
   e 0= if s exit then                                 \ +/-0 / f64-subnormal -> signed 0
   x 1 < if
      s 1 52 lshift m or 43 x - IEEE754:ROUND-SHIFT-EVEN or exit
   then
   x 30 > if s $7C00 or exit then                      \ overflow -> +/-inf
   m 42 rshift {: mt :}                                \ top 10 mantissa bits
   m $3FFFFFFFFFF and {: rem :}                         \ the 42 dropped bits
   rem $20000000000 > if 1 else
      rem $20000000000 = if mt 1 and else 0 then
   then {: inc :}                                      \ round-to-nearest-even increment
   x 10 lshift mt or inc + {: v :}                     \ carry from mantissa bumps exponent
   v $7BFF > if s $7C00 or exit then                   \ carry overflowed to inf
   s v or ;
: SF-ST16 ( n ptr u8 -- ) {: v:n p:ptr :}          \ store low 16 bits of v LE at p
   v          $FF and  p     c!
   v 8 rshift $FF and  p 1 + c! ;
: F16-PACK ( ptr r n ptr u8 -- ) {: src:ptr cnt:n dst:ptr :}   \ n f64 cells -> f16
   cnt 0 ?do  src i cells + @ F64>F16  dst i 2 * +  SF-ST16  loop ;

\ --- bf16 (brain float) narrowing for the bf16 mma tile (dot habu-bf16-m16n8k16-tile). bf16 is
\ sign(1) + exp(8, bias 127) + mantissa(7): its exponent field is IDENTICAL to f32's (same bias and
\ range), so a bf16 is exactly an f32 with the low 16 mantissa bits removed. F64>BF16 therefore
\ mirrors F32:NARROW, NOT F64>F16: the target exponent is x = e-896 (the f32/f64 offset, NOT f16's
\ e-1008), and the overflow bound (x>254) and subnormal boundary (x<1) are f32's, not f16's much
\ smaller range - only the stored mantissa width (7 vs 23) and the 16-bit store differ. This is what
\ "round via the f32-representable value" means: bf16 inherits f32's exponent field, so its range
\ handling is F32:NARROW's. Rounding is round-to-nearest-even done in ONE step directly on the 52-bit
\ f64 mantissa (keep the top 7 bits, RNE the 45 dropped bits) = the correctly-rounded nearest bf16.
\ It is deliberately NOT f64->f32->bf16 (drop 29 then 16 bits): that double rounding can mis-round a
\ value sitting exactly on an f32 rounding boundary, so a single rounding to the final 7-bit width is
\ the correct pack. Truncation is NOT used. The subnormal shift 46-x = 53-7-x reuses the width-agnostic
\ RNE shift (a carry out of the 7 mantissa bits lands on 0x0080, the smallest bf16 normal).
\ SF-ST16 stores the low 16 bits little-endian; device C stays f32 (F32-BUF:UNPACK on readback), so no
\ BF16-UNPACK is needed. For the mma-gemm-check integer fills (<=256, exact in bf16's 8-bit
\ significand) no rounding fires and the pack returns the exact integer. ---
: F64>BF16 ( r -- n ) {: fr:r :}
   fr IEEE754:F64>BITS {: b:n :}
   b 63 rshift 1 and 15 lshift {: s:n :}               \ sign in bf16 bit 15
   b 52 rshift $7FF and {: e:n :}                        \ f64 biased exponent
   b $FFFFFFFFFFFFF and {: m:n :}                         \ 52-bit mantissa
   e 896 - {: x:n :}                                      \ target bf16 biased exponent (f32 bias-127 field)
   e $7FF = if                                            \ inf / NaN
      m 0= if s $7F80 or exit then                        \ +/-inf
      s $7F80 or  m 45 rshift or  $40 or exit             \ quiet NaN (payload kept)
   then
   e 0= if s exit then                                    \ +/-0 / f64-subnormal -> signed 0
   x 1 < if
      s 1 52 lshift m or 46 x - IEEE754:ROUND-SHIFT-EVEN or exit
   then
   x 254 > if s $7F80 or exit then                        \ overflow -> +/-inf
   m 45 rshift {: mt:n :}                                 \ top 7 mantissa bits
   m $1FFFFFFFFFFF and {: rem:n :}                         \ the 45 dropped bits
   rem $100000000000 > if 1 else
      rem $100000000000 = if mt 1 and else 0 then
   then {: inc:n :}                                       \ round-to-nearest-even increment
   x 7 lshift mt or inc + {: v:n :}                       \ carry from mantissa bumps exponent
   v $7F7F > if s $7F80 or exit then                      \ carry overflowed to inf
   s v or ;
: BF16-PACK ( ptr r n ptr u8 -- ) {: src:ptr cnt:n dst:ptr :}   \ n f64 cells -> bf16
   cnt 0 ?do  src i cells + @ F64>BF16  dst i 2 * +  SF-ST16  loop ;

\ --- per-op emitters (operate on register numbers) ---
\ GRID-CTX: global flat index + bounds predicate; returns the byte-offset rd reg.
: EMIT-GRID-CTX ( n -- n ) {: spanrd :}    \ span base unused (index is from tid)
   CG-NEXT-R {: rc :}  CG-NEXT-R {: rn :}  CG-NEXT-R {: rt :}  CG-NEXT-R {: ri :}
   SB-RESET s" mov.u32 " CG-S rc CG-R s" , %ctaid.x;" CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rn CG-R s" , %ntid.x;"  CG-S CG-LINE
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;"   CG-S CG-LINE
   SB-RESET s" mad.lo.u32 " CG-S ri CG-R s" , " CG-S rc CG-R s" , " CG-S rn CG-R s" , " CG-S rt CG-R s" ;" CG-S CG-LINE
   ri CG-GRID-IDX !                        \ expose the flat index for per-input index-remap folds
   CG-NEXT-P {: p :}
   SB-RESET s" setp.ge.u32 " CG-S p CG-P s" , " CG-S ri CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  bra DONE;" CG-S CG-LINE
   CG-NEXT-RD {: off :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S ri CG-R s" , 4;" CG-S CG-LINE
   off ;

\ GRID-INDEX: the flat output index register EMIT-GRID-CTX just computed (%rN). A folded
\ staged transpose remaps its load off this index instead of the coalesced byte offset.
: EMIT-GRID-INDEX ( -- n )  CG-GRID-IDX @ ;

\ XPOSE-OFF: the byte offset of a transpose-folded source element. Given the flat output
\ index reg and the transpose dims (dstC = output cols, srcC = source cols), dst[i,j]=src[j,i]
\ maps flat e -> src_flat = (e mod dstC)*srcC + e/dstC; returns the source byte-offset rd reg.
\ dstC/srcC are emit-time constants, so they ride as immediates (no extra kernel params).
: EMIT-XPOSE-OFF ( n n n -- n ) {: flatr:n dstC:n srcC:n :}
   CG-NEXT-R {: rj:n :}
   SB-RESET s" rem.u32 " CG-S rj CG-R s" , " CG-S flatr CG-R s" , " CG-S dstC FMT:SB-U s" ;" CG-S CG-LINE
   CG-NEXT-R {: ri:n :}
   SB-RESET s" div.u32 " CG-S ri CG-R s" , " CG-S flatr CG-R s" , " CG-S dstC FMT:SB-U s" ;" CG-S CG-LINE
   CG-NEXT-R {: rsrc:n :}
   SB-RESET s" mad.lo.u32 " CG-S rsrc CG-R s" , " CG-S rj CG-R s" , " CG-S srcC FMT:SB-U s" , " CG-S ri CG-R s" ;" CG-S CG-LINE
   CG-NEXT-RD {: off:n :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S rsrc CG-R s" , 4;" CG-S CG-LINE
   off ;

\ --- broadcast-operand load offsets (mirror the executor EX-BC@ element mapping) ---
\ A region input whose shape is a legal broadcast of the region shape loads a REMAPPED
\ flat index off the shared grid index reg; C (the region output cols) is an emit-time
\ immediate, so the remap needs no extra kernel param. See maki/bcast.f for the classes.
\ MOD-OFF: a 1xC row-broadcast reads element (e mod C) -> byte offset (e mod C)*4.
: EMIT-MOD-OFF ( n n -- n ) {: flatr:n C:n :}
   CG-NEXT-R {: rj:n :}
   SB-RESET s" rem.u32 " CG-S rj CG-R s" , " CG-S flatr CG-R s" , " CG-S C FMT:SB-U s" ;" CG-S CG-LINE
   CG-NEXT-RD {: off:n :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S rj CG-R s" , 4;" CG-S CG-LINE
   off ;
\ DIV-OFF: an Rx1 col-broadcast reads element (e / C) -> byte offset (e / C)*4.
: EMIT-DIV-OFF ( n n -- n ) {: flatr:n C:n :}
   CG-NEXT-R {: ri:n :}
   SB-RESET s" div.u32 " CG-S ri CG-R s" , " CG-S flatr CG-R s" , " CG-S C FMT:SB-U s" ;" CG-S CG-LINE
   CG-NEXT-RD {: off:n :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S ri CG-R s" , 4;" CG-S CG-LINE
   off ;
\ ZERO-OFF: a 1x1 scalar broadcast reads element 0 -> a constant zero byte offset.
: EMIT-ZERO-OFF ( -- n )
   CG-NEXT-RD {: off:n :}
   SB-RESET s" mov.u64 " CG-S off CG-RD s" , 0;" CG-S CG-LINE
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
