\ cuda-launch.f - fail-closed CUDA launch proof for a checked SAXPY cubin.

require lib/ptx/cuda-driver.f

package CUDALAUNCH

$3F800000 constant X0-BITS
$40000000 constant X1-BITS
$40400000 constant X2-BITS
$40800000 constant X3-BITS
$41200000 constant Y0-BITS
$41A00000 constant Y1-BITS
$41F00000 constant Y2-BITS
$42200000 constant Y3-BITS
$40400000 constant A-BITS
$41500000 constant GOLD0-BITS
$41D00000 constant GOLD1-BITS
$421C0000 constant GOLD2-BITS
$42500000 constant GOLD3-BITS
$DEADBEEF constant READ-SENTINEL

create X-BUF 16 allot
create Y-BUF 16 allot
create R-BUF 16 allot

variable DEV
variable CTX
variable MOD
variable FUNC
variable DX
variable DY
variable ABITS
variable NV

: U32! ( n ptr u8 n -- )
   {: value:n buf:ptr idx:n :}
   idx 4 * {: off:n :}
   value $FF and buf off + c!
   value 8 rshift $FF and buf off 1 + + c!
   value 16 rshift $FF and buf off 2 + + c!
   value 24 rshift $FF and buf off 3 + + c! ;

: U32@ ( ptr u8 n -- n )
   {: buf:ptr idx:n :}
   idx 4 * {: off:n :}
   buf off + c@
   buf off 1 + + c@ 8 lshift or
   buf off 2 + + c@ 16 lshift or
   buf off 3 + + c@ 24 lshift or ;

: RESET ( -- )
   0 DEV !
   0 CTX !
   0 MOD !
   0 FUNC !
   0 DX !
   0 DY ! ;

: INPUTS! ( -- )
   X0-BITS X-BUF 0 U32!
   X1-BITS X-BUF 1 U32!
   X2-BITS X-BUF 2 U32!
   X3-BITS X-BUF 3 U32!
   Y0-BITS Y-BUF 0 U32!
   Y1-BITS Y-BUF 1 U32!
   Y2-BITS Y-BUF 2 U32!
   Y3-BITS Y-BUF 3 U32!
   READ-SENTINEL R-BUF 0 U32!
   READ-SENTINEL R-BUF 1 U32!
   READ-SENTINEL R-BUF 2 U32!
   READ-SENTINEL R-BUF 3 U32! ;

: SETUP ( -- )
   CUDA:RESET
   CUDA:INIT
   DEV CUDA:DEVICE-GET
   CTX DEV @ CUDA:PRIMARY-CTX-RETAIN
   CTX @ CUDA:CTX-CURRENT! ;

: LOAD-SAXPY ( -- )
   s" /tmp/saxpy.cubin" MOD CUDA:LOAD-MODULE
   MOD @ s" SAXPY" FUNC CUDA:MODULE-FUNCTION ;

: ALLOC ( -- )
   16 DX CUDA:DEVICE-ALLOC
   16 DY CUDA:DEVICE-ALLOC
   DX @ X-BUF 16 CUDA:HTOD
   DY @ Y-BUF 16 CUDA:HTOD ;

: PARAMS ( -- )
   A-BITS ABITS !
   4 NV !
   FUNC @ 256 1 1 CUDA:BLOCK-SHAPE
   FUNC @ 24 CUDA:PARAM-SIZE
   FUNC @ 0 DX CUDA:PARAM-PTR!
   FUNC @ 8 DY CUDA:PARAM-PTR!
   FUNC @ 16 ABITS CUDA:PARAM-U32!
   FUNC @ 20 NV CUDA:PARAM-U32! ;

: LAUNCH ( -- )
   FUNC @ 1 1 CUDA:LAUNCH-GRID
   CUDA:SYNC
   R-BUF DY @ 16 CUDA:DTOH ;

: FREE ( -- )
   DX @ CUDA:DEVICE-FREE
   DY @ CUDA:DEVICE-FREE
   0 DX !
   0 DY ! ;

: RELEASE ( -- )
   FREE
   MOD @ CUDA:UNLOAD-MODULE
   CTX @ 0 <> if DEV @ CUDA:PRIMARY-CTX-RELEASE then
   RESET ;

: CHECK-ELEM ( n n -- )
   {: idx:n want:n :}
   R-BUF idx U32@ want CUDA:EXPECT-GOLDEN ;

: CHECK-OUTPUT ( -- )
   0 GOLD0-BITS CHECK-ELEM
   1 GOLD1-BITS CHECK-ELEM
   2 GOLD2-BITS CHECK-ELEM
   3 GOLD3-BITS CHECK-ELEM ;

: BODY ( -- )
   INPUTS!
   SETUP
   LOAD-SAXPY
   ALLOC
   PARAMS
   LAUNCH ;

: RUN ( -- )
   RESET
   [: BODY ;] catch {: rc:n :}
   RELEASE
   rc 0 <> if rc throw then
   CHECK-OUTPUT
   s" SAXPY on GPU: nonuniform 4-lane golden verified" type cr ;

RUN

end-package
