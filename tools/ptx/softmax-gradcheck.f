\ softmax-gradcheck.f - device finite-difference gradcheck of SOFTMAX-ROWS backward.
\
\ Prereqs: /tmp/softmax.cubin and /tmp/softmax-bwd.cubin.

require lib/test.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/launch.f
require tools/ptx/bench.f

package PTXSOFTMAXGC

4 constant GCK
16 constant GC-BYTES

create GC-IN GC-BYTES allot
create GC-OUT GC-BYTES allot
create GC-DYB GC-BYTES allot
create HX 4 cells allot
create HDY 4 cells allot
create HDXA 4 cells allot
create HDXN 4 cells allot
create HYP 4 cells allot
create HYM 4 cells allot

variable GC-DX
variable GC-DDY
variable GC-DO
variable GC-KV

: GC-T-AT ( ptr a n -- ptr a )
   cells + ;

: GC-T-GET ( ptr a n -- r )
   GC-T-AT @ ;

: GC-T-SET ( r ptr a n -- )
   GC-T-AT ! ;

: F32! ( n ptr u8 n -- )
   {: v:n buf:ptr idx:n :}
   idx 4 * {: off:n :}
   v $FF and buf off + c!
   v 8 rshift $FF and buf off 1 + + c!
   v 16 rshift $FF and buf off 2 + + c!
   v 24 rshift $FF and buf off 3 + + c! ;

: F32@ ( ptr u8 n -- n )
   {: buf:ptr idx:n :}
   idx 4 * {: off:n :}
   buf off + c@
   buf off 1 + + c@ 8 lshift or
   buf off 2 + + c@ 16 lshift or
   buf off 3 + + c@ 24 lshift or ;

: PACK4 ( ptr a ptr u8 -- )
   {: src:ptr dst:ptr :}
   GCK 0 ?do src i GC-T-GET F64>F32 dst i F32! loop ;

: UNPACK4 ( ptr u8 ptr a -- )
   {: src:ptr dst:ptr :}
   GCK 0 ?do src i F32@ F32>F64 dst i GC-T-SET loop ;

: GC-SETUP ( -- )
   PTX:BENCH-RESET
   1 PTX:BENCH-GRID!
   256 PTX:BENCH-BLOCK!
   PTX:DEVICE-OPEN
   GC-BYTES GC-DX PTX:DEVICE-ALLOC
   GC-BYTES GC-DDY PTX:DEVICE-ALLOC
   GC-BYTES GC-DO PTX:DEVICE-ALLOC
   GCK GC-KV ! ;

: GC-LOAD-FWD ( -- )
   PTX:MODULE-UNLOAD
   s" /tmp/softmax.cubin" PTX:BENCH-CUBIN!
   s" SOFTMAX_ROWS" PTX:BENCH-KERNEL!
   20 PTX:KERNEL-PARAM-BYTES!
   PTX:MODULE-LOAD
   PTX:KERNEL-PREPARE-LAUNCH ;

: GC-LOAD-BWD ( -- )
   PTX:MODULE-UNLOAD
   s" /tmp/softmax-bwd.cubin" PTX:BENCH-CUBIN!
   s" SOFTMAX_BWD" PTX:BENCH-KERNEL!
   28 PTX:KERNEL-PARAM-BYTES!
   PTX:MODULE-LOAD
   PTX:KERNEL-PREPARE-LAUNCH ;

: GC-FWD-PARAMS ( -- )
   0 GC-DX PTX:KERNEL-PARAM-PTR!
   8 GC-DO PTX:KERNEL-PARAM-PTR!
   16 GC-KV PTX:KERNEL-PARAM-U32! ;

: GC-BWD-PARAMS ( -- )
   0 GC-DX PTX:KERNEL-PARAM-PTR!
   8 GC-DDY PTX:KERNEL-PARAM-PTR!
   16 GC-DO PTX:KERNEL-PARAM-PTR!
   24 GC-KV PTX:KERNEL-PARAM-U32! ;

: GC-FWD-RUN ( ptr a ptr a -- )
   {: src:ptr dst:ptr :}
   1 GCK 256 PTX-ROW-LAUNCH-CHECK
   GC-LOAD-FWD
   src GC-IN PACK4
   GC-DX @ GC-IN GC-BYTES PTX:HTOD
   GC-FWD-PARAMS
   PTX:KERNEL-LAUNCH
   PTX:DEVICE-SYNC
   GC-OUT GC-DO @ GC-BYTES PTX:DTOH
   GC-OUT dst UNPACK4 ;

: GC-BWD-RUN ( -- )
   1 GCK 256 PTX-ROW-LAUNCH-CHECK
   GC-LOAD-BWD
   HX GC-IN PACK4
   HDY GC-DYB PACK4
   GC-DX @ GC-IN GC-BYTES PTX:HTOD
   GC-DDY @ GC-DYB GC-BYTES PTX:HTOD
   GC-BWD-PARAMS
   PTX:KERNEL-LAUNCH
   PTX:DEVICE-SYNC
   GC-OUT GC-DO @ GC-BYTES PTX:DTOH
   GC-OUT HDXA UNPACK4 ;

: FREE-DEV ( n -- )
   dup 0 <> if PTX:DEVICE-FREE else drop then ;

: GC-RELEASE ( -- )
   PTX:MODULE-UNLOAD
   GC-DX @ FREE-DEV
   GC-DDY @ FREE-DEV
   GC-DO @ FREE-DEV
   0 GC-DX ! 0 GC-DDY ! 0 GC-DO !
   PTX:DEVICE-CLOSE ;

: GC-EPS ( -- r )
   1.0 4096.0 f/ ;

: GC-NUM-J ( n -- r )
   {: jx:n :}
   HX jx GC-T-GET {: x0:r :}
   x0 GC-EPS f+ HX jx GC-T-SET
   HX HYP GC-FWD-RUN
   x0 GC-EPS f- HX jx GC-T-SET
   HX HYM GC-FWD-RUN
   x0 HX jx GC-T-SET
   0.0 GCK 0 ?do HDY i GC-T-GET HYP i GC-T-GET HYM i GC-T-GET f- f* f+ loop
   GC-EPS 2.0 f* f/ ;

: GC-RUN ( -- )
   1.0 HX 0 GC-T-SET
   2.0 HX 1 GC-T-SET
   0.5 HX 2 GC-T-SET
   1.5 HX 3 GC-T-SET
   0.1 HDY 0 GC-T-SET
   0.2 HDY 1 GC-T-SET
   0.3 HDY 2 GC-T-SET
   0.4 HDY 3 GC-T-SET
   GC-SETUP
   GCK 0 ?do i GC-NUM-J HDXN i GC-T-SET loop
   GC-BWD-RUN
   GC-RELEASE
   GCK 0 ?do HDXN i GC-T-GET HDXA i GC-T-GET f- fabs 1.0 100.0 f/ f< TTRUE loop ;

T-RESET
GC-RUN
T-REPORT

end-package
