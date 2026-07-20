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
require lib/ptx/toolchain.f
require lib/ptx/cuda-scope.f
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
create RB 4 allot

variable PIDX
variable PX
variable POUT
variable PDX
variable PDZ
variable NIDX
variable NDATA

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
   s" lib/ptx/tile.f" >LEN PROC-ARGV+ ;

: RUN-EMIT ( -- result<pcap:captured,pcap:failed> )
   s" bin/hb" >LEN
   EMIT-OUT OUT-CAP >LEN
   EMIT-ERR ERR-CAP >LEN
   30000 >MS RUN-ARGV-CAPTURE ;

: EMIT-WRITE ( len len rc -- n n ) {: o:len e:len c:rc :}   \ err-len unused; write PTX, return out-bytes + code
   PTXTC:PTX$ EMIT-OUT o LEN>N WRITE-ALL
   o LEN>N c RC>N ;
: EMIT-INDEXED ( -- n n )
   EMIT-PRELUDE
   s" tools/ptx/indexed-scatter-cg.f" >LEN PROC-ARGV+
   RUN-EMIT MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC EMIT-WRITE ENDOF
     err OF PCAP-FAILED:UNMAKE EMIT-WRITE ENDOF
   ;MATCH ;

: RUN-PTXAS ( -- n )
   PTXAS-OUT ERR-CAP >LEN PTXAS-ERR ERR-CAP >LEN PTXTC:ASSEMBLE ;

: PTXAS-INDEXED ( -- n )
   RUN-PTXAS ;

: GRID-N ( -- n )
   INDEX-N BLOCK-N 1- + BLOCK-N / ;

: SETUP ( -- )
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   s" INDEXED-SCATTER-GRADCHECK" PTXBENCH:LABEL!
   BLOCK-N PTXBENCH:BLOCK!
   PARAM-BYTES PTXBENCH:PARAM-BYTES!
   GRID-N PTXBENCH:GRID!
   PTXBENCH:OPEN  PTXBENCH:OWN-CTX                   \ scope owns the primary context...
   INDEX-N WORD-BYTES * PIDX PTXBENCH:DEVICE-ALLOC  PIDX PTXBENCH:OWN-DEV   \ ...and each device buffer
   WORD-BYTES PX PTXBENCH:DEVICE-ALLOC   PX PTXBENCH:OWN-DEV
   WORD-BYTES POUT PTXBENCH:DEVICE-ALLOC POUT PTXBENCH:OWN-DEV
   WORD-BYTES PDX PTXBENCH:DEVICE-ALLOC  PDX PTXBENCH:OWN-DEV
   WORD-BYTES PDZ PTXBENCH:DEVICE-ALLOC  PDZ PTXBENCH:OWN-DEV ;

: LOAD-KERNEL ( ptr u8 n -- )
   PTXBENCH:UNLOAD
   PTXBENCH:KERNEL!
   PTXBENCH:LOAD
   PTXBENCH:PREPARE-LAUNCH ;

: READ-DEV ( n -- n )
   RB swap 4 PTXBENCH:DTOH
   RB U32@ ;

: ZERO-IDX ( -- )
   PIDX @ 0 INDEX-N PTXBENCH:DEVICE-MEMSET32 ;

: PARAM-SIZES ( -- )
   INDEX-N NIDX !
   DATA-N NDATA ! ;

: PARAMS-FWD ( -- )
   PARAM-SIZES
   0 PIDX PTXBENCH:PARAM-PTR!
   8 PX PTXBENCH:PARAM-PTR!
   16 POUT PTXBENCH:PARAM-PTR!
   24 NIDX PTXBENCH:PARAM-U32!
   28 NDATA PTXBENCH:PARAM-U32! ;

: PARAMS-BWD ( -- )
   PARAM-SIZES
   0 PIDX PTXBENCH:PARAM-PTR!
   8 PDX PTXBENCH:PARAM-PTR!
   16 PDZ PTXBENCH:PARAM-PTR!
   24 NIDX PTXBENCH:PARAM-U32!
   28 NDATA PTXBENCH:PARAM-U32! ;

: RUN-FWD ( n -- n )
   ZERO-IDX
   PX @ swap 1 PTXBENCH:DEVICE-MEMSET32
   POUT @ 0 1 PTXBENCH:DEVICE-MEMSET32
   s" INDEXED_FWD" LOAD-KERNEL
   PARAMS-FWD
   PTXBENCH:LAUNCH
   PTXBENCH:SYNC
   POUT @ READ-DEV ;

: RUN-BWD ( -- n )
   ZERO-IDX
   PDX @ 0 1 PTXBENCH:DEVICE-MEMSET32
   PDZ @ DZ-BITS 1 PTXBENCH:DEVICE-MEMSET32
   s" INDEXED_BWD" LOAD-KERNEL
   PARAMS-BWD
   PTXBENCH:LAUNCH
   PTXBENCH:SYNC
   PDX @ READ-DEV ;

: CENTRAL ( -- r )
   XPLUS-BITS RUN-FWD F32>F64 {: zp:r :}
   XMINUS-BITS RUN-FWD F32>F64 {: zm:r :}
   zp zm f- 2.0 f/ ;

: ABS-DIFF ( r r -- r )
   f- dup 0.0 f< if 0.0 swap f- then ;

: NEAR? ( r r -- bool )
   ABS-DIFF 0.05 f< ;

\ device body under the run scope: SETUP owns ctx + buffers; the module is the mutable
\ reload slot (PTXBENCH:LOAD/UNLOAD), unloaded here on success. A throw unwinds ctx + buffers.
: RUN-BODY ( -- )
   SETUP
   CENTRAL {: num:r :}
   RUN-BWD {: analytic:n :}
   analytic EXPECTED-BITS T=
   analytic F32>F64 num NEAR? TTRUE
   PTXBENCH:UNLOAD ;

: MAIN ( -- )
   T-RESET
   s" habu-ptx-indexed" PTXTC:PREPARE
   EMIT-INDEXED {: outn:n erc:n :}
   erc 0 T=
   outn 0 > TTRUE
   PTXAS-INDEXED 0 T=
   [: RUN-BODY ;] CUDA-SCOPE:SCOPE
   PTXTC:CLEAN
   s" device gradcheck: indexed scatter-add duplicates verified for n=1024" type cr
   T-REPORT ;

MAIN

;package
