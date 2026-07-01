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
create V4-ROOT FS-PATH-CAP allot
create V4-PTX FS-PATH-CAP allot
create V4-CUBIN FS-PATH-CAP allot
create RB 4 allot

variable V4-ROOT-U
variable V4-PTX-U
variable V4-CUBIN-U
variable DX
variable DY
variable A
variable NVAR

: V4-ROOT$ ( -- ptr u8 n )
   V4-ROOT V4-ROOT-U @ ;

: V4-PTX$ ( -- ptr u8 n )
   V4-PTX V4-PTX-U @ ;

: V4-CUBIN$ ( -- ptr u8 n )
   V4-CUBIN V4-CUBIN-U @ ;

: PREPARE-PATHS ( -- )
   CLEANUP-RESET
   s" habu-saxpy-v4-tail" V4-ROOT V4-ROOT-U PTX:TEMP-DIR!
   V4-ROOT$ s" saxpy-v4-tail.ptx" V4-PTX V4-PTX-U PTX:JOIN-PATH!
   V4-ROOT$ s" saxpy-v4-tail.cubin" V4-CUBIN V4-CUBIN-U PTX:JOIN-PATH! ;

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
   V4-PTX$ EMIT-OUT outu LEN>N WRITE-ALL
   outu LEN>N rc RC>N ;

: PTXAS-V4-SAXPY ( -- n )
   V4-PTX$ V4-CUBIN$ PTXAS-OUT ERR-CAP PTXAS-ERR ERR-CAP PTX:PTXAS-RUN-DEFAULT
   {: outu:n erru:n rc:n :}
   rc ;

: GRID-FOR ( n -- n )
   V4-ELEMS-PER-BLOCK 1- + V4-ELEMS-PER-BLOCK / ;

: BYTES-FOR ( n -- n )
   1+ 4 * ;

: SETUP ( -- )
   PTX:BENCH-RESET
   V4-CUBIN$ PTX:BENCH-CUBIN!
   s" SAXPY" PTX:BENCH-KERNEL!
   s" SAXPY-V4-TAIL" PTX:BENCH-LABEL!
   V4-BLOCK PTX:BENCH-BLOCK!
   24 PTX:KERNEL-PARAM-BYTES!
   PTX:DEVICE-OPEN
   PTX:MODULE-LOAD ;

: RELEASE ( -- )
   PTX:MODULE-UNLOAD
   PTX:DEVICE-CLOSE ;

: ALLOC-N ( n -- )
   {: n:n :}
   n BYTES-FOR {: bytes:n :}
   bytes DX PTX:DEVICE-ALLOC
   bytes DY PTX:DEVICE-ALLOC
   DX @ X-BITS n 1+ PTX:DEVICE-MEMSET32
   DY @ SENTINEL-BITS n 1+ PTX:DEVICE-MEMSET32
   DY @ 0 n PTX:DEVICE-MEMSET32 ;

: FREE-N ( -- )
   DX @ 0 <> if
      DX @ PTX:DEVICE-FREE
   then
   DY @ 0 <> if
      DY @ PTX:DEVICE-FREE
   then
   0 DX ! 0 DY ! ;

: PARAMS-N ( n -- )
   {: n:n :}
   n NVAR !
   A-BITS A !
   n GRID-FOR PTX:BENCH-GRID!
   PTX:KERNEL-PREPARE-LAUNCH
   0 DX PTX:KERNEL-PARAM-PTR!
   8 DY PTX:KERNEL-PARAM-PTR!
   16 A PTX:KERNEL-PARAM-U32!
   20 NVAR PTX:KERNEL-PARAM-U32! ;

: CHECK-ELEM ( n n -- )
   {: idx:n want:n :}
   RB DY @ idx 4 * + 4 PTX:DTOH
   RB PTX:U32@ want T= ;

: CHECK-N ( n -- )
   {: n:n :}
   n ALLOC-N
   n PARAMS-N
   PTX:KERNEL-LAUNCH
   PTX:DEVICE-SYNC
   0 Y-BITS CHECK-ELEM
   n 1- Y-BITS CHECK-ELEM
   n SENTINEL-BITS CHECK-ELEM
   FREE-N ;

: MAIN ( -- )
   T-RESET
   PREPARE-PATHS
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
   CLEANUP-RUN
   s" device: SAXPY-V4 scalar residual tail verified for n=4,5,7,1000003" type cr
   T-REPORT ;

MAIN

end-package
