\ saxpy-v4-tail-device-test.f - Orin device proof for v4 scalar residual lanes.
\
\ Emits the checked v4 SAXPY kernel, assembles it with ptxas, then launches
\ n=4,5,7,1000003. The output buffer has one sentinel element beyond n; the test
\ proves active lanes write 6.0f and the first inactive lane remains 1.0f.

require lib/test.f
require lib/fs.f
require lib/process-argv.f
require tools/ptx/bench.f

package PTXV4TAIL

$8000 constant OUT-CAP
$1000 constant ERR-CAP
256 constant V4-BLOCK
1024 constant V4-ELEMS-PER-BLOCK
$40000000 constant X-BITS
$3F800000 constant SENTINEL-BITS
$40400000 constant A-BITS
$40C00000 constant Y-BITS

create EMIT-OUT OUT-CAP allot
create EMIT-ERR ERR-CAP allot
create PTXAS-OUT ERR-CAP allot
create PTXAS-ERR ERR-CAP allot
create RB 4 allot

variable DX
variable DY
variable A
variable NVAR

: U32@ ( ptr u8 -- n )
   dup c@
   over 1 BYTE+ c@ 8 lshift or
   over 2 BYTE+ c@ 16 lshift or
   swap 3 BYTE+ c@ 24 lshift or ;

: EMIT-PRELUDE ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+
   s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f" >LEN PROC-ARGV+
   s" lib/fmt.f" >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f" >LEN PROC-ARGV+
   s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f" >LEN PROC-ARGV+
   s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/cg-vec.f" >LEN PROC-ARGV+
   s" lib/ptx/tile.f" >LEN PROC-ARGV+
   s" lib/ptx/tile-v4.f" >LEN PROC-ARGV+ ;

: RUN-EMIT ( -- len len rc )
   s" bin/hb" >LEN
   EMIT-OUT OUT-CAP >LEN
   EMIT-ERR ERR-CAP >LEN
   30000 >MS RUN-ARGV-CAPTURE ;

: EMIT-V4-SAXPY ( -- n n )
   EMIT-PRELUDE
   s" tools/ptx/saxpy-v4-cg.f" >LEN PROC-ARGV+
   RUN-EMIT {: outu:len erru:len rc:rc :}
   s" /tmp/saxpy-v4-tail.ptx" EMIT-OUT outu LEN>N WRITE-ALL
   outu LEN>N rc RC>N ;

: RUN-PTXAS ( -- len len rc )
   s" /usr/local/cuda-12.6/bin/ptxas" >LEN
   PTXAS-OUT ERR-CAP >LEN
   PTXAS-ERR ERR-CAP >LEN
   10000 >MS RUN-ARGV-CAPTURE ;

: PTXAS-V4-SAXPY ( -- n )
   PROC-ARGV-RESET
   s" -arch=sm_87" >LEN PROC-ARGV+
   s" /tmp/saxpy-v4-tail.ptx" >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   s" /tmp/saxpy-v4-tail.cubin" >LEN PROC-ARGV+
   RUN-PTXAS {: outu:len erru:len rc:rc :}
   rc RC>N ;

: GRID-FOR ( n -- n )
   V4-ELEMS-PER-BLOCK 1- + V4-ELEMS-PER-BLOCK / ;

: BYTES-FOR ( n -- n )
   1+ 4 * ;

: SETUP ( -- )
   PTXBENCH:RESET
   s" /tmp/saxpy-v4-tail.cubin" PTXBENCH:CUBIN!
   s" SAXPY" PTXBENCH:KERNEL!
   s" SAXPY-V4-TAIL" PTXBENCH:LABEL!
   V4-BLOCK PTXBENCH:BLOCK!
   24 PTXBENCH:PARAM-BYTES!
   PTXBENCH:OPEN
   PTXBENCH:LOAD ;

: RELEASE ( -- )
   PTXBENCH:UNLOAD
   PTXBENCH:CLOSE ;

: ALLOC-N ( n -- )
   {: n:n :}
   n BYTES-FOR {: bytes:n :}
   bytes DX PTXBENCH:DEVICE-ALLOC
   bytes DY PTXBENCH:DEVICE-ALLOC
   DX @ X-BITS n 1+ PTXBENCH:DEVICE-MEMSET32
   DY @ SENTINEL-BITS n 1+ PTXBENCH:DEVICE-MEMSET32
   DY @ 0 n PTXBENCH:DEVICE-MEMSET32 ;

: FREE-N ( -- )
   DX @ 0 <> if
      DX @ PTXBENCH:DEVICE-FREE
   then
   DY @ 0 <> if
      DY @ PTXBENCH:DEVICE-FREE
   then
   0 DX ! 0 DY ! ;

: PARAMS-N ( n -- )
   {: n:n :}
   n NVAR !
   A-BITS A !
   n GRID-FOR PTXBENCH:GRID!
   PTXBENCH:PREPARE-LAUNCH
   0 DX PTXBENCH:PARAM-PTR!
   8 DY PTXBENCH:PARAM-PTR!
   16 A PTXBENCH:PARAM-U32!
   20 NVAR PTXBENCH:PARAM-U32! ;

: CHECK-ELEM ( n n -- )
   {: idx:n want:n :}
   RB DY @ idx 4 * + 4 PTXBENCH:DTOH
   RB U32@ want T= ;

: CHECK-N ( n -- )
   {: n:n :}
   n ALLOC-N
   n PARAMS-N
   PTXBENCH:LAUNCH
   PTXBENCH:SYNC
   0 Y-BITS CHECK-ELEM
   n 1- Y-BITS CHECK-ELEM
   n SENTINEL-BITS CHECK-ELEM
   FREE-N ;

: MAIN ( -- )
   T-RESET
   EMIT-V4-SAXPY {: outn:n erc:n :}
   erc 0 T=
   outn 0 > TTRUE
   PTXAS-V4-SAXPY 0 T=
   SETUP
   4 CHECK-N
   5 CHECK-N
   7 CHECK-N
   1000003 CHECK-N
   RELEASE
   s" device: SAXPY-V4 scalar residual tail verified for n=4,5,7,1000003" type cr
   T-REPORT ;

MAIN

end-package
