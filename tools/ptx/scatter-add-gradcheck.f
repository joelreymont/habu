\ scatter-add-gradcheck.f - device gradcheck for scatter-add accumulation.
\
\ Multi-block fan-in: f(x) = sum_i x[0].  The finite-difference gradient is n,
\ and the analytic backward must accumulate n cotangents into dx[0].

require lib/test.f
require lib/fs.f
require lib/process-argv.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require tools/ptx/bench.f

package PTXSCATTERGRAD

$8000 constant OUT-CAP
$1000 constant ERR-CAP
256 constant BLOCK-N
1024 constant FANIN-N
4 constant WORD-BYTES
20 constant PARAM-BYTES
$40400000 constant XPLUS-BITS
$3F800000 constant XMINUS-BITS
$3F800000 constant DZ-BITS
$44800000 constant EXPECTED-BITS

create EMIT-OUT OUT-CAP allot
create EMIT-ERR ERR-CAP allot
create PTXAS-OUT ERR-CAP allot
create PTXAS-ERR ERR-CAP allot
create SC-ROOT FS-PATH-CAP allot
create SC-PTX FS-PATH-CAP allot
create SC-CUBIN FS-PATH-CAP allot
create RB 4 allot

variable SC-ROOT-U
variable SC-PTX-U
variable SC-CUBIN-U
variable PX
variable POUT
variable PDX
variable PDZ
variable NVAR

: SC-ROOT$ ( -- ptr u8 n )
   SC-ROOT SC-ROOT-U @ ;

: SC-PTX$ ( -- ptr u8 n )
   SC-PTX SC-PTX-U @ ;

: SC-CUBIN$ ( -- ptr u8 n )
   SC-CUBIN SC-CUBIN-U @ ;

: PREPARE-PATHS ( -- )
   CLEANUP-RESET
   s" habu-scatter-add-grad" SC-ROOT SC-ROOT-U PTX:TEMP-DIR!
   SC-ROOT$ s" scatter-add-grad.ptx" SC-PTX SC-PTX-U PTX:JOIN-PATH!
   SC-ROOT$ s" scatter-add-grad.cubin" SC-CUBIN SC-CUBIN-U PTX:JOIN-PATH! ;

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
   s" lib/ptx/tile.f" >LEN PROC-ARGV+ ;

: RUN-EMIT ( -- len len rc )
   s" bin/hb" >LEN
   EMIT-OUT OUT-CAP >LEN
   EMIT-ERR ERR-CAP >LEN
   30000 >MS RUN-ARGV-CAPTURE ;

: EMIT-FANIN ( -- n n )
   EMIT-PRELUDE
   s" tools/ptx/scatter-add-grad-cg.f" >LEN PROC-ARGV+
   RUN-EMIT {: outu:len erru:len rc:rc :}
   SC-PTX$ EMIT-OUT outu LEN>N WRITE-ALL
   outu LEN>N rc RC>N ;

: PTXAS-FANIN ( -- n )
   SC-PTX$ SC-CUBIN$ PTXAS-OUT ERR-CAP PTXAS-ERR ERR-CAP PTX:PTXAS-RUN-DEFAULT
   {: outu:n erru:n rc:n :}
   rc ;

: GRID-N ( -- n )
   FANIN-N BLOCK-N 1- + BLOCK-N / ;

: SETUP ( -- )
   PTX:BENCH-RESET
   SC-CUBIN$ PTX:BENCH-CUBIN!
   s" SCATTER-ADD-GRADCHECK" PTX:BENCH-LABEL!
   BLOCK-N PTX:BENCH-BLOCK!
   PARAM-BYTES PTX:KERNEL-PARAM-BYTES!
   GRID-N PTX:BENCH-GRID!
   PTX:DEVICE-OPEN
   WORD-BYTES PX PTX:DEVICE-ALLOC
   WORD-BYTES POUT PTX:DEVICE-ALLOC
   WORD-BYTES PDX PTX:DEVICE-ALLOC
   WORD-BYTES PDZ PTX:DEVICE-ALLOC ;

: FREE-DEV ( n -- )
   dup 0 <> if
      PTX:DEVICE-FREE
   else
      drop
   then ;

: RELEASE ( -- )
   PTX:MODULE-UNLOAD
   PX @ FREE-DEV
   POUT @ FREE-DEV
   PDX @ FREE-DEV
   PDZ @ FREE-DEV
   0 PX ! 0 POUT ! 0 PDX ! 0 PDZ !
   PTX:DEVICE-CLOSE ;

: LOAD-KERNEL ( ptr u8 n -- )
   PTX:MODULE-UNLOAD
   PTX:BENCH-KERNEL!
   PTX:MODULE-LOAD
   PTX:KERNEL-PREPARE-LAUNCH ;

: READ-DEV ( n -- n )
   RB swap 4 PTX:DTOH
   RB PTX:U32@ ;

: PARAMS-FWD ( -- )
   FANIN-N NVAR !
   0 PX PTX:KERNEL-PARAM-PTR!
   8 POUT PTX:KERNEL-PARAM-PTR!
   16 NVAR PTX:KERNEL-PARAM-U32! ;

: PARAMS-BWD ( -- )
   FANIN-N NVAR !
   0 PDX PTX:KERNEL-PARAM-PTR!
   8 PDZ PTX:KERNEL-PARAM-PTR!
   16 NVAR PTX:KERNEL-PARAM-U32! ;

: RUN-FWD ( n -- n )
   PX @ swap 1 PTX:DEVICE-MEMSET32
   POUT @ 0 1 PTX:DEVICE-MEMSET32
   s" FANIN_FWD" LOAD-KERNEL
   PARAMS-FWD
   PTX:KERNEL-LAUNCH
   PTX:DEVICE-SYNC
   POUT @ READ-DEV ;

: RUN-BWD ( -- n )
   PDX @ 0 1 PTX:DEVICE-MEMSET32
   PDZ @ DZ-BITS 1 PTX:DEVICE-MEMSET32
   s" FANIN_BWD" LOAD-KERNEL
   PARAMS-BWD
   PTX:KERNEL-LAUNCH
   PTX:DEVICE-SYNC
   PDX @ READ-DEV ;

: CENTRAL ( -- r )
   XPLUS-BITS RUN-FWD F32>F64 {: zp:r :}
   XMINUS-BITS RUN-FWD F32>F64 {: zm:r :}
   zp zm f- 2.0 f/ ;

: ABS-DIFF ( r r -- r )
   f- dup 0.0 f< if 0.0 swap f- then ;

: NEAR? ( r r -- bool )
   ABS-DIFF 0.05 f< ;

: MAIN ( -- )
   T-RESET
   PREPARE-PATHS
   EMIT-FANIN {: outn:n erc:n :}
   erc 0 T=
   outn 0 > TTRUE
   PTXAS-FANIN 0 T=
   SETUP
   CENTRAL {: num:r :}
   RUN-BWD {: analytic:n :}
   analytic EXPECTED-BITS T=
   analytic F32>F64 num NEAR? TTRUE
   RELEASE
   CLEANUP-RUN
   s" device gradcheck: scatter-add fan-in accumulation verified for n=1024 across 4 blocks" type cr
   T-REPORT ;

MAIN

end-package
