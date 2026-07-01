\ eval-device-sm.f - device-golden autograder for softmax-rows.
\
\ GRADE-SM grades a softmax candidate by checker -> emit PTX -> ptxas ->
\ CUDA Driver launch -> softmax golden.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/float.f
require lib/fmt.f
require src/core/combinators.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/launch.f
require maki/eval.f
require tools/ptx/bench.f

package MAKI

$8000 constant GSP-CAP
$1000 constant GSQ-CAP

create SM-IN 16 allot
create SM-OUT 16 allot
create SMG 4 cells allot
create SM-ROOT FS-PATH-CAP allot
create SM-DRIVER FS-PATH-CAP allot
create SM-PTX FS-PATH-CAP allot
create SM-CUBIN FS-PATH-CAP allot
create GSP-OUT GSP-CAP allot
create GSP-ERR GSQ-CAP allot
create GSQ-OUT GSQ-CAP allot
create GSQ-ERR GSQ-CAP allot

variable SM-ROOT-U
variable SM-DRIVER-U
variable SM-PTX-U
variable SM-CUBIN-U
variable SM-DI
variable SM-DO
variable SM-KV
variable SM-OK

: SM-ROOT$ ( -- ptr u8 n )
   SM-ROOT SM-ROOT-U @ ;

: SM-DRIVER$ ( -- ptr u8 n )
   SM-DRIVER SM-DRIVER-U @ ;

: SM-PTX$ ( -- ptr u8 n )
   SM-PTX SM-PTX-U @ ;

: SM-CUBIN$ ( -- ptr u8 n )
   SM-CUBIN SM-CUBIN-U @ ;

: SM-F32! ( n ptr u8 n -- )
   {: v:n buf:ptr idx:n :}
   idx 4 * {: off:n :}
   v $FF and buf off + c!
   v 8 rshift $FF and buf off 1 + + c!
   v 16 rshift $FF and buf off 2 + + c!
   v 24 rshift $FF and buf off 3 + + c! ;

: SM-F32@ ( n -- n )
   {: idx:n :}
   idx 4 * {: off:n :}
   SM-OUT off + c@
   SM-OUT off 1 + + c@ 8 lshift or
   SM-OUT off 2 + + c@ 16 lshift or
   SM-OUT off 3 + + c@ 24 lshift or ;

: SM-INIT ( -- )
   1.0 F64>F32 SM-IN 0 SM-F32!
   2.0 F64>F32 SM-IN 1 SM-F32!
   3.0 F64>F32 SM-IN 2 SM-F32!
   4.0 F64>F32 SM-IN 3 SM-F32!
   1023627234 SMG 0 cells + !
   1035106489 SMG 1 cells + !
   1047695721 SMG 2 cells + !
   1059379089 SMG 3 cells + ! ;

: GRADE-SM-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-grade-softmax" SM-ROOT SM-ROOT-U PTX:TEMP-DIR!
   SM-ROOT$ s" grade-sm-driver.f" SM-DRIVER SM-DRIVER-U PTX:JOIN-PATH!
   SM-ROOT$ s" grade-sm.ptx" SM-PTX SM-PTX-U PTX:JOIN-PATH!
   SM-ROOT$ s" grade-sm.cubin" SM-CUBIN SM-CUBIN-U PTX:JOIN-PATH! ;

: SM-SETUP ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   PTX:BENCH-RESET
   path pathu PTX:BENCH-CUBIN!
   s" SOFTMAX_ROWS" PTX:BENCH-KERNEL!
   s" SOFTMAX_ROWS" PTX:BENCH-LABEL!
   256 PTX:BENCH-BLOCK!
   1 PTX:BENCH-GRID!
   20 PTX:KERNEL-PARAM-BYTES!
   PTX:DEVICE-OPEN
   PTX:MODULE-LOAD ;

: SM-ALLOC ( -- )
   16 SM-DI PTX:DEVICE-ALLOC
   16 SM-DO PTX:DEVICE-ALLOC
   SM-DI @ SM-IN 16 PTX:HTOD
   SM-DO @ $7FC00000 4 PTX:DEVICE-MEMSET32 ;

: SM-PARAMS ( -- )
   4 SM-KV !
   PTX:KERNEL-PREPARE-LAUNCH
   0 SM-DI PTX:KERNEL-PARAM-PTR!
   8 SM-DO PTX:KERNEL-PARAM-PTR!
   16 SM-KV PTX:KERNEL-PARAM-U32! ;

: SM-LAUNCH ( -- )
   SM-ALLOC
   SM-PARAMS
   PTX:KERNEL-LAUNCH
   PTX:DEVICE-SYNC
   SM-OUT SM-DO @ 16 PTX:DTOH ;

: SM-FREE-DEV ( n -- )
   dup 0 <> if PTX:DEVICE-FREE else drop then ;

: SM-RELEASE ( -- )
   PTX:MODULE-UNLOAD
   SM-DI @ SM-FREE-DEV
   SM-DO @ SM-FREE-DEV
   0 SM-DI ! 0 SM-DO !
   PTX:DEVICE-CLOSE ;

: SM-RUN ( ptr u8 n -- )
   1 4 256 PTX-ROW-LAUNCH-CHECK
   SM-SETUP
   SM-LAUNCH
   SM-RELEASE ;

: SM-NEAR? ( n n -- bool )
   - abs 8 <= ;

: DEVICE-CORRECT-SM? ( ptr u8 n -- bool )
   SM-RUN
   0 SM-F32@ SMG 0 cells + @ SM-NEAR?
   1 SM-F32@ SMG 1 cells + @ SM-NEAR? and
   2 SM-F32@ SMG 2 cells + @ SM-NEAR? and
   3 SM-F32@ SMG 3 cells + @ SM-NEAR? and ;

: SM-CHECK-ACT ( -- )
   SM-CUBIN$ DEVICE-CORRECT-SM? SM-OK ! ;

: SM-CHECK-RC ( -- n )
   0 SM-OK !
   [: SM-CHECK-ACT ;] catch ;

: SM-CHECKED-VERDICT ( -- n )
   SM-CHECK-RC {: rc:n :}
   rc 0= if SM-OK @ if 2 else 1 then exit then
   rc E-PTX-CUDA-DRIVER = if 1 exit then
   rc E-PTX-DEVICE-WRONG = if 1 exit then
   rc throw ;

: GRADE-SM-WRITE-DRIVER ( ptr u8 n -- )
   {: a:ptr u:n :}
   SB-RESET
   s" 256 %BLOCK" SB-APPEND 10 SB-APPEND-C
   s" : " SB-APPEND a u SB-APPEND s"  ;" SB-APPEND 10 SB-APPEND-C
   s" CG-SM-RESET CG-HEADER CG-SM-ENTRY CG-SM-OPEN CG-SM-PARAMS 1 MATRIX-REG 2 MATRIX-REG K CG-SM-RET CG-SM-CLOSE" SB-APPEND 10 SB-APPEND-C
   s" bye" SB-APPEND 10 SB-APPEND-C
   SM-DRIVER$ SB$ WRITE-ALL ;

: GRADE-SM-EMIT ( -- n )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+
   s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f" >LEN PROC-ARGV+
   s" lib/fmt.f" >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f" >LEN PROC-ARGV+
   s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f" >LEN PROC-ARGV+
   s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/collective.f" >LEN PROC-ARGV+
   SM-DRIVER$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN GSP-OUT GSP-CAP >LEN GSP-ERR GSQ-CAP >LEN 20000 >MS RUN-ARGV-ENV-CAPTURE
   {: outu:len erru:len rc:rc :}
   SM-PTX$ GSP-OUT outu LEN>N WRITE-ALL
   outu LEN>N ;

: GRADE-SM-PTXAS ( -- n )
   SM-PTX$ SM-CUBIN$ GSQ-OUT GSQ-CAP GSQ-ERR GSQ-CAP PTX:PTXAS-RUN-DEFAULT
   {: outu:n erru:n rc:n :}
   rc ;

public

: GRADE-SM ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u CHECK-PASSES? 0= if 0 exit then
   GRADE-SM-PREPARE
   a u GRADE-SM-WRITE-DRIVER
   GRADE-SM-EMIT 0 = if CLEANUP-RUN 1 exit then
   GRADE-SM-PTXAS 0 <> if CLEANUP-RUN 1 exit then
   SM-CHECKED-VERDICT
   CLEANUP-RUN ;

SM-INIT

end-package
