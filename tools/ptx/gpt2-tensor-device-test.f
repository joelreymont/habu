\ Real-CUDA parity for the six fixed GPT-2 tensor entries.

require lib/errors.f
require lib/float.f
require lib/float32.f
require lib/fs.f
require lib/test.f
require lib/ptx/gpt2-tensor.f
require lib/ptx/toolchain.f
require lib/ptx/cuda-driver.f
require lib/ptx/cuda-scope.f
require lib/ptx/sentinel.f
require maki/array.f
require maki/gelu.f
require maki/layernorm.f
require maki/linear.f
require maki/eval/active-target.f

package GPT2-PTX-DEVICE-TEST

using F32

private

768 constant NH
2304 constant NQ
3072 constant NF
50257 constant NV
NV NH * 4 * constant WB
NV 4 * constant OB
$C0E80000 constant TAIL-BITS

create P0 OB allot  create P1 OB allot
create P2 OB allot  create P3 OB 4 + allot
create A0 4096 cells allot  create A1 4096 cells allot
create A2 4096 cells allot  create A3 4096 cells allot
create PATH 128 allot  create KN 64 allot
create QO $1000 allot  create QE $1000 allot

variable DEV  variable CTX  variable MF
variable D0   variable D1   variable D2   variable D3
variable FE   variable FLN  variable FLI
variable FU   variable FG   variable FR
variable E0   variable E1   variable E2

: U32! ( n ptr u8 n -- ) {: v:n p:ptr i:n :}
   i 4 * {: o:n :}
   v $FF and p o + c!  v 8 rshift $FF and p o 1 + + c!
   v 16 rshift $FF and p o 2 + + c!  v 24 rshift $FF and p o 3 + + c! ;

: U32@ ( ptr u8 n -- n ) {: p:ptr i:n :}
   i 4 * {: o:n :}
   p o + c@  p o 1 + + c@ 8 lshift or
   p o 2 + + c@ 16 lshift or  p o 3 + + c@ 24 lshift or ;

: A! ( r ptr a n -- ) {: v:r p:ptr i:n :} v p i cells + ! ;
: A@ ( ptr a n -- r ) {: p:ptr i:n :} p i cells + @ ;

: PACK ( ptr a ptr u8 n -- ) {: a:ptr p:ptr n:n :}
   n 0 ?do a i A@ NARROW p i U32! loop ;

: NEAR? ( r r -- bool ) {: a:r b:r :}
   a b f- fabs 0.002 f< ;

: CHECK-A ( ptr a n -- ) {: a:ptr n:n :}
   n 0 ?do P3 i U32@ WIDEN a i A@ NARROW WIDEN NEAR? TTRUE loop ;

: CHECK-C ( r n -- ) {: x:r n:n :}
   n 0 ?do P3 i U32@ WIDEN x NEAR? TTRUE loop ;

: PSET ( cuda-fn n ptr u8 n -- ) {: fn:cuda-fn off:n p:ptr bytes:n :}
   fn off >IDX p bytes >LEN CUDA:CU-PARAM-SET-V CUDA:RC0 ;

: GO ( cuda-fn n -- ) {: fn:cuda-fn grid:n :}
   fn 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   fn grid 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0 ;

: GRID ( n -- n ) 255 + 256 / ;

: COPY ( n n -- ) {: d:n n:n :}
   P3 d >CUDA-DEVPTR n 1+ 4 * >LEN CUDA:DTOH ;

: GUARD ( n n -- ) {: d:n n:n :}
   d n 4 * + >CUDA-DEVPTR TAIL-BITS 1 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0 ;

: CHECK-TAIL ( n -- )
   P3 swap U32@ TAIL-BITS = TTRUE ;

: POISON ( n n -- ) {: d:n n:n :}
   d >CUDA-DEVPTR PTXSENT:POISON n >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0 ;

: MEMSET ( n r n -- ) {: d:n x:r n:n :}
   d >CUDA-DEVPTR x NARROW n >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0 ;

: LAUNCH-EMBED ( n n n -- ) E2 ! E1 ! E0 !
   FE @ >CUDA-FN {: fn:cuda-fn :}
   fn 44 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   fn 0 D0 8 PSET  fn 8 D1 8 PSET  fn 16 D2 8 PSET  fn 24 D3 8 PSET
   fn 32 E0 4 PSET  fn 36 E1 4 PSET
   fn 40 E2 4 PSET
   fn E0 @ E1 @ * GRID GO ;

: LAUNCH-LN ( n n -- ) E1 ! E0 !
   FLN @ >CUDA-FN {: fn:cuda-fn :}
   fn 40 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   fn 0 D0 8 PSET  fn 8 D1 8 PSET  fn 16 D2 8 PSET  fn 24 D3 8 PSET
   fn 32 E0 4 PSET  fn 36 E1 4 PSET
   fn E0 @ GO ;

: LAUNCH-LINEAR ( n n n -- ) E2 ! E1 ! E0 !
   FLI @ >CUDA-FN {: fn:cuda-fn :}
   fn 44 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   fn 0 D0 8 PSET  fn 8 D1 8 PSET  fn 16 D2 8 PSET  fn 24 D3 8 PSET
   fn 32 E0 4 PSET  fn 36 E1 4 PSET  fn 40 E2 4 PSET
   fn E0 @ E2 @ * GRID GO ;

: LAUNCH-UNEMBED ( n n -- ) E1 ! E0 !
   FU @ >CUDA-FN {: fn:cuda-fn :}
   fn 32 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   fn 0 D0 8 PSET  fn 8 D1 8 PSET  fn 16 D3 8 PSET
   fn 24 E0 4 PSET  fn 28 E1 4 PSET
   fn E1 @ GRID GO ;

: LAUNCH-GELU ( n -- ) E0 !
   FG @ >CUDA-FN {: fn:cuda-fn :}
   fn 12 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   fn 0 D0 8 PSET  fn 8 E0 4 PSET
   fn E0 @ GRID GO ;

: LAUNCH-RESIDUAL ( n -- ) E0 !
   FR @ >CUDA-FN {: fn:cuda-fn :}
   fn 28 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   fn 0 D0 8 PSET  fn 8 D1 8 PSET  fn 16 D3 8 PSET  fn 24 E0 4 PSET
   fn E0 @ GRID GO ;

: ALLOC ( ptr a n -- ) {: dst:ptr bytes:n :}
   dst bytes >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   dst @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR ;

: BUILD ( -- )
   s" habu-gpt2-tensor" PTXTC:PREPARE
   ATGT:LABEL$ PTX-ARCH!  ATGT:VER$ PTX-VER!
   32 %BLOCK
   PTX-CAPTURE-ON GPT2-PTX:EMIT PTX-CAPTURE-OFF
   PTX-BLOCK@ 256 T=
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL
   ATGT:LABEL$ PTXTC:TC-ARCH!
   QO $1000 >LEN QE $1000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT 0 T= ;

: SETUP ( -- )
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   CTX DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   DEV @ >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ PATH FFI:CSTR
   MF PATH CUDA:CU-MODULE-LOAD CUDA:RC0
   MF @ >CUDA-MOD CUDA-SCOPE:OWN-MODULE
   D0 16384 ALLOC  D1 WB ALLOC  D2 OB ALLOC  D3 OB 4 + ALLOC
   s" GPT2_EMBED" KN FFI:CSTR
   FE MF @ >CUDA-MOD KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   s" GPT2_LAYERNORM" KN FFI:CSTR
   FLN MF @ >CUDA-MOD KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   s" GPT2_LINEAR" KN FFI:CSTR
   FLI MF @ >CUDA-MOD KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   s" GPT2_UNEMBED" KN FFI:CSTR
   FU MF @ >CUDA-MOD KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   s" GPT2_GELU" KN FFI:CSTR
   FG MF @ >CUDA-MOD KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   s" GPT2_RESIDUAL" KN FFI:CSTR
   FR MF @ >CUDA-MOD KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0 ;

: TEST-EMBED ( -- )
   2 P0 0 U32!  0 P0 1 U32!  3 P0 2 U32!
   4 NH * 0 ?do  i NH / 16 * i NH mod 31 mod + s>f NARROW P1 i U32! loop
   8 NH * 0 ?do  i NH / 8 * i NH mod 7 mod + s>f NARROW P2 i U32! loop
   D0 @ >CUDA-DEVPTR P0 12 >LEN CUDA:HTOD
   D1 @ >CUDA-DEVPTR P1 4 NH * 4 * >LEN CUDA:HTOD
   D2 @ >CUDA-DEVPTR P2 8 NH * 4 * >LEN CUDA:HTOD
   D3 @ 3 NH * POISON  D3 @ 3 NH * GUARD
   3 NH 5 LAUNCH-EMBED
   D3 @ 3 NH * COPY
   3 NH * 0 ?do
      P0 i NH / U32@ 16 * i NH mod 31 mod +
      i NH / 5 + 8 * i NH mod 7 mod + + s>f
      P3 i U32@ WIDEN swap NEAR? TTRUE
   loop
   3 NH * CHECK-TAIL ;

: EMBED-TAIL ( -- )
   D0 @ 0.0 256 MEMSET
   D1 @ 2.0 1 MEMSET
   D2 @ 3.0 257 MEMSET
   D3 @ 1 POISON  D3 @ 1 GUARD
   1 1 1 LAUNCH-EMBED
   D3 @ 1 COPY
   5.0 1 CHECK-C
   1 CHECK-TAIL ;

: LN-RUN ( n n -- ) {: rows:n cols:n :}
   cols 0 ?do
      i 7 mod 1+ s>f 4.0 f/ A1 i A!
      i 5 mod 2 - s>f 8.0 f/ A2 i A!
   loop
   rows 0 ?do
      A0 i cols * cells +  A3 i cols * cells +  A1 A2 cols MAKI:LN-AFFINE-FWD
   loop
   A0 P0 rows cols * PACK  A1 P1 cols PACK  A2 P2 cols PACK
   D0 @ >CUDA-DEVPTR P0 rows cols * 4 * >LEN CUDA:HTOD
   D1 @ >CUDA-DEVPTR P1 cols 4 * >LEN CUDA:HTOD
   D2 @ >CUDA-DEVPTR P2 cols 4 * >LEN CUDA:HTOD
   D3 @ rows cols * POISON  D3 @ rows cols * GUARD
   rows cols LAUNCH-LN
   D3 @ rows cols * COPY
   A3 rows cols * CHECK-A
   rows cols * CHECK-TAIL ;

: LN-CASE ( n n -- ) {: rows:n cols:n :}
   rows cols * 0 ?do  i 7 mod 3 - s>f A0 i A! loop
   rows cols LN-RUN ;

: LN-EPS ( -- )
   NH 0 ?do  i 1 and 0<> if 2.00390625 else 1.99609375 then A0 i A! loop
   1 NH LN-RUN ;

: TEST-LINEAR-SMALL ( -- )
   10 0 ?do i 5 mod 2 - s>f A0 i A! loop
   35 0 ?do i 7 mod 3 - s>f 4.0 f/ A1 i A! loop
   7 0 ?do i s>f 8.0 f/ A2 i A! loop
   A0 A1 A2 A3 2 5 7 MAKI:LINEAR
   A0 P0 10 PACK  A1 P1 35 PACK  A2 P2 7 PACK
   D0 @ >CUDA-DEVPTR P0 40 >LEN CUDA:HTOD
   D1 @ >CUDA-DEVPTR P1 140 >LEN CUDA:HTOD
   D2 @ >CUDA-DEVPTR P2 28 >LEN CUDA:HTOD
   D3 @ 14 POISON  D3 @ 14 GUARD
   2 5 7 LAUNCH-LINEAR
   D3 @ 14 COPY
   A3 14 CHECK-A
   14 CHECK-TAIL ;

: LINEAR-LARGE ( n n -- ) {: inner:n cols:n :}
   D3 @ cols POISON  D3 @ cols GUARD
   1 inner cols LAUNCH-LINEAR
   D3 @ cols COPY
   inner s>f 1.0 f+ cols CHECK-C
   cols CHECK-TAIL ;

: TEST-LINEAR-LARGE ( -- )
   D0 @ 1.0 NF MEMSET
   D1 @ 1.0 NH NF * MEMSET
   D2 @ 1.0 NF MEMSET
   NH NQ LINEAR-LARGE  NH NF LINEAR-LARGE  NF NH LINEAR-LARGE ;

: TEST-UNEMBED-SMALL ( -- )
   5 0 ?do i 1+ s>f 4.0 f/ A0 i A! loop
   259 5 * 0 ?do i 13 mod 6 - s>f 8.0 f/ A1 i A! loop
   259 0 ?do
      0.0  5 0 ?do A0 i A@ A1 j 5 * i + A@ f* f+ loop  A2 i A!
   loop
   A0 P0 5 PACK  A1 P1 1295 PACK
   D0 @ >CUDA-DEVPTR P0 20 >LEN CUDA:HTOD
   D1 @ >CUDA-DEVPTR P1 5180 >LEN CUDA:HTOD
   D3 @ 259 POISON  D3 @ 259 GUARD
   5 259 LAUNCH-UNEMBED
   D3 @ 259 COPY
   A2 259 CHECK-A
   259 CHECK-TAIL ;

: TEST-UNEMBED-LARGE ( -- )
   D0 @ 1.0 NH MEMSET
   D1 @ 1.0 NV NH * MEMSET
   D3 @ NV POISON  D3 @ NV GUARD
   NH NV LAUNCH-UNEMBED
   D3 @ NV COPY
   768.0 NV CHECK-C
   NV CHECK-TAIL ;

: GELU-CASE ( n -- ) {: n:n :}
   n 0 ?do i 11 mod 5 - s>f 2.0 f/ dup A0 i A! MAKI:GELU-F A1 i A! loop
   A0 P0 n PACK
   D0 @ >CUDA-DEVPTR P0 n 4 * >LEN CUDA:HTOD
   D0 @ n GUARD
   n LAUNCH-GELU
   D0 @ n COPY
   A1 n CHECK-A
   n CHECK-TAIL ;

: RESIDUAL-CASE ( n -- ) {: n:n :}
   n 0 ?do
      i 17 mod 8 - s>f 4.0 f/ dup A0 i A!
      i 7 mod 3 - s>f 8.0 f/ dup A1 i A! f+ A2 i A!
   loop
   A0 P0 n PACK  A1 P1 n PACK
   D0 @ >CUDA-DEVPTR P0 n 4 * >LEN CUDA:HTOD
   D1 @ >CUDA-DEVPTR P1 n 4 * >LEN CUDA:HTOD
   D3 @ n POISON  D3 @ n GUARD
   n LAUNCH-RESIDUAL
   D3 @ n COPY
   A2 n CHECK-A
   n CHECK-TAIL ;

: RUN ( -- )
   TEST-EMBED  EMBED-TAIL
   2 5 LN-CASE  1 NH LN-CASE  LN-EPS
   TEST-LINEAR-SMALL  TEST-LINEAR-LARGE
   TEST-UNEMBED-SMALL  TEST-UNEMBED-LARGE
   NF GELU-CASE  1 GELU-CASE
   NH RESIDUAL-CASE  1 RESIDUAL-CASE ;

: MAIN ( -- )
   T-RESET
   BUILD
   [: SETUP RUN ;] CUDA-SCOPE:SCOPE
   PTXTC:CLEAN
   T-REPORT ;

MAIN

;using
;package
