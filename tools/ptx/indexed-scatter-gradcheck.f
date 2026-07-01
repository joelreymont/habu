\ indexed-scatter-gradcheck.f - device gradcheck for indexed scatter-add.
\
\ Duplicate-index gather/scatter: f(x) = sum_i x[idx[i]] with idx[i] = 0.
\ The finite-difference gradient and the analytic backward both accumulate n
\ cotangents into dx[0].

require lib/test.f
require lib/fs.f
require lib/process-argv.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require tools/ptx/bench.f

package PTXINDEXGRAD

$9000 constant OUT-CAP
$1000 constant ERR-CAP
256 constant BLOCK-N
1024 constant INDEX-N
1 constant DATA-N
4 constant WORD-BYTES
32 constant PARAM-BYTES
$40400000 constant XPLUS-BITS
$3F800000 constant XMINUS-BITS
$3F800000 constant DZ-BITS
$44800000 constant EXPECTED-BITS

create EMIT-OUT OUT-CAP allot
create EMIT-ERR ERR-CAP allot
create PTXAS-OUT ERR-CAP allot
create PTXAS-ERR ERR-CAP allot
create IDX-ROOT FS-PATH-CAP allot
create IDX-PTX FS-PATH-CAP allot
create IDX-CUBIN FS-PATH-CAP allot
create RB 4 allot

variable IDX-ROOT-U
variable IDX-PTX-U
variable IDX-CUBIN-U
variable PIDX
variable PX
variable POUT
variable PDX
variable PDZ
variable NIDX
variable NDATA

: IDX-ROOT$ ( -- ptr u8 n )
   IDX-ROOT IDX-ROOT-U @ ;

: IDX-PTX$ ( -- ptr u8 n )
   IDX-PTX IDX-PTX-U @ ;

: IDX-CUBIN$ ( -- ptr u8 n )
   IDX-CUBIN IDX-CUBIN-U @ ;

: PREPARE-PATHS ( -- )
   CLEANUP-RESET
   s" habu-indexed-scatter" IDX-ROOT IDX-ROOT-U PTX:TEMP-DIR!
   IDX-ROOT$ s" indexed-scatter.ptx" IDX-PTX IDX-PTX-U PTX:JOIN-PATH!
   IDX-ROOT$ s" indexed-scatter.cubin" IDX-CUBIN IDX-CUBIN-U PTX:JOIN-PATH! ;

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

: EMIT-INDEXED ( -- n n )
   EMIT-PRELUDE
   s" tools/ptx/indexed-scatter-cg.f" >LEN PROC-ARGV+
   RUN-EMIT {: outu:len erru:len rc:rc :}
   IDX-PTX$ EMIT-OUT outu LEN>N WRITE-ALL
   outu LEN>N rc RC>N ;

: PTXAS-INDEXED ( -- n )
   IDX-PTX$ IDX-CUBIN$ PTXAS-OUT ERR-CAP PTXAS-ERR ERR-CAP PTX:PTXAS-RUN-DEFAULT
   {: outu:n erru:n rc:n :}
   rc ;

: GRID-N ( -- n )
   INDEX-N BLOCK-N 1- + BLOCK-N / ;

: SETUP ( -- )
   PTX:BENCH-RESET
   IDX-CUBIN$ PTX:BENCH-CUBIN!
   s" INDEXED-SCATTER-GRADCHECK" PTX:BENCH-LABEL!
   BLOCK-N PTX:BENCH-BLOCK!
   PARAM-BYTES PTX:KERNEL-PARAM-BYTES!
   GRID-N PTX:BENCH-GRID!
   PTX:DEVICE-OPEN
   INDEX-N WORD-BYTES * PIDX PTX:DEVICE-ALLOC
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
   PIDX @ FREE-DEV
   PX @ FREE-DEV
   POUT @ FREE-DEV
   PDX @ FREE-DEV
   PDZ @ FREE-DEV
   0 PIDX ! 0 PX ! 0 POUT ! 0 PDX ! 0 PDZ !
   PTX:DEVICE-CLOSE ;

: LOAD-KERNEL ( ptr u8 n -- )
   PTX:MODULE-UNLOAD
   PTX:BENCH-KERNEL!
   PTX:MODULE-LOAD
   PTX:KERNEL-PREPARE-LAUNCH ;

: READ-DEV ( n -- n )
   RB swap 4 PTX:DTOH
   RB PTX:U32@ ;

: ZERO-IDX ( -- )
   PIDX @ 0 INDEX-N PTX:DEVICE-MEMSET32 ;

: PARAM-SIZES ( -- )
   INDEX-N NIDX !
   DATA-N NDATA ! ;

: PARAMS-FWD ( -- )
   PARAM-SIZES
   0 PIDX PTX:KERNEL-PARAM-PTR!
   8 PX PTX:KERNEL-PARAM-PTR!
   16 POUT PTX:KERNEL-PARAM-PTR!
   24 NIDX PTX:KERNEL-PARAM-U32!
   28 NDATA PTX:KERNEL-PARAM-U32! ;

: PARAMS-BWD ( -- )
   PARAM-SIZES
   0 PIDX PTX:KERNEL-PARAM-PTR!
   8 PDX PTX:KERNEL-PARAM-PTR!
   16 PDZ PTX:KERNEL-PARAM-PTR!
   24 NIDX PTX:KERNEL-PARAM-U32!
   28 NDATA PTX:KERNEL-PARAM-U32! ;

: RUN-FWD ( n -- n )
   ZERO-IDX
   PX @ swap 1 PTX:DEVICE-MEMSET32
   POUT @ 0 1 PTX:DEVICE-MEMSET32
   s" INDEXED_FWD" LOAD-KERNEL
   PARAMS-FWD
   PTX:KERNEL-LAUNCH
   PTX:DEVICE-SYNC
   POUT @ READ-DEV ;

: RUN-BWD ( -- n )
   ZERO-IDX
   PDX @ 0 1 PTX:DEVICE-MEMSET32
   PDZ @ DZ-BITS 1 PTX:DEVICE-MEMSET32
   s" INDEXED_BWD" LOAD-KERNEL
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
   EMIT-INDEXED {: outn:n erc:n :}
   erc 0 T=
   outn 0 > TTRUE
   PTXAS-INDEXED 0 T=
   SETUP
   CENTRAL {: num:r :}
   RUN-BWD {: analytic:n :}
   analytic EXPECTED-BITS T=
   analytic F32>F64 num NEAR? TTRUE
   RELEASE
   CLEANUP-RUN
   s" device gradcheck: indexed scatter-add duplicates verified for n=1024" type cr
   T-REPORT ;

MAIN

end-package
