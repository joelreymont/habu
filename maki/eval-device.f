\ eval-device.f - device-golden SAXPY autograder.
\
\ GRADE-CANDIDATE grades certify AND run-correct: checker -> emit PTX -> ptxas
\ -> CUDA Driver launch -> CPU golden.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require src/core/combinators.f
require maki/eval.f
require tools/ptx/bench.f

package MAKI

$4000 constant GP-CAP
$1000 constant GQ-CAP

create ED-ROOT FS-PATH-CAP allot
create ED-DRIVER FS-PATH-CAP allot
create ED-PTX FS-PATH-CAP allot
create ED-CUBIN FS-PATH-CAP allot
create GP-OUT GP-CAP allot
create GP-ERR GQ-CAP allot
create GQ-OUT GQ-CAP allot
create GQ-ERR GQ-CAP allot
create ED-RBUF 4 allot

variable ED-ROOT-U
variable ED-DRIVER-U
variable ED-PTX-U
variable ED-CUBIN-U
variable ED-DX
variable ED-DY
variable ED-AB
variable ED-NV
variable ED-OK

: ED-ROOT$ ( -- ptr u8 n )
   ED-ROOT ED-ROOT-U @ ;

: ED-DRIVER$ ( -- ptr u8 n )
   ED-DRIVER ED-DRIVER-U @ ;

: ED-PTX$ ( -- ptr u8 n )
   ED-PTX ED-PTX-U @ ;

: ED-CUBIN$ ( -- ptr u8 n )
   ED-CUBIN ED-CUBIN-U @ ;

: GRADE-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-grade-saxpy" ED-ROOT ED-ROOT-U PTX:TEMP-DIR!
   ED-ROOT$ s" grade-driver.f" ED-DRIVER ED-DRIVER-U PTX:JOIN-PATH!
   ED-ROOT$ s" grade.ptx" ED-PTX ED-PTX-U PTX:JOIN-PATH!
   ED-ROOT$ s" grade.cubin" ED-CUBIN ED-CUBIN-U PTX:JOIN-PATH! ;

: ED-SETUP ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   PTX:BENCH-RESET
   path pathu PTX:BENCH-CUBIN!
   s" SAXPY" PTX:BENCH-KERNEL!
   s" SAXPY" PTX:BENCH-LABEL!
   256 PTX:BENCH-BLOCK!
   1 PTX:BENCH-GRID!
   24 PTX:KERNEL-PARAM-BYTES!
   PTX:DEVICE-OPEN
   PTX:MODULE-LOAD ;

: ED-ALLOC ( -- )
   16 ED-DX PTX:DEVICE-ALLOC
   16 ED-DY PTX:DEVICE-ALLOC
   ED-DX @ $40000000 4 PTX:DEVICE-MEMSET32
   ED-DY @ 0 4 PTX:DEVICE-MEMSET32 ;

: ED-PARAMS ( -- )
   $40400000 ED-AB !
   4 ED-NV !
   PTX:KERNEL-PREPARE-LAUNCH
   0 ED-DX PTX:KERNEL-PARAM-PTR!
   8 ED-DY PTX:KERNEL-PARAM-PTR!
   16 ED-AB PTX:KERNEL-PARAM-U32!
   20 ED-NV PTX:KERNEL-PARAM-U32! ;

: ED-LAUNCH ( -- )
   ED-ALLOC
   ED-PARAMS
   PTX:KERNEL-LAUNCH
   PTX:DEVICE-SYNC
   ED-RBUF ED-DY @ 4 PTX:DTOH ;

: ED-FREE-DEV ( n -- )
   dup 0 <> if PTX:DEVICE-FREE else drop then ;

: ED-RELEASE ( -- )
   PTX:MODULE-UNLOAD
   ED-DX @ ED-FREE-DEV
   ED-DY @ ED-FREE-DEV
   0 ED-DX ! 0 ED-DY !
   PTX:DEVICE-CLOSE ;

: ED-RUN ( ptr u8 n -- n )
   $DEADBEEF ED-RBUF PTX:U32!
   ED-SETUP
   ED-LAUNCH
   ED-RELEASE
   ED-RBUF PTX:U32@ ;

: DEVICE-CORRECT? ( ptr u8 n -- bool )
   ED-RUN $40C00000 = ;

public

0 constant EVN-EMIT-FAIL
1 constant EVN-PTXAS-FAIL
2 constant EVN-DEVICE-WRONG
3 constant EVN-GREEN

private

: ED-CHECK-ACT ( -- )
   ED-CUBIN$ DEVICE-CORRECT? ED-OK ! ;

: ED-CHECK-RC ( -- n )
   0 ED-OK !
   [: ED-CHECK-ACT ;] catch ;

: ED-CHECKED-VERDICT ( -- n )
   ED-CHECK-RC {: rc:n :}
   rc 0= if ED-OK @ if 2 else 1 then exit then
   rc E-PTX-CUDA-DRIVER = if 1 exit then
   rc E-PTX-DEVICE-WRONG = if 1 exit then
   rc throw ;

: ED-NOCHECK-VERDICT ( -- n )
   ED-CHECK-RC {: rc:n :}
   rc 0= if ED-OK @ if EVN-GREEN else EVN-DEVICE-WRONG then exit then
   rc E-PTX-CUDA-DRIVER = if EVN-DEVICE-WRONG exit then
   rc E-PTX-DEVICE-WRONG = if EVN-DEVICE-WRONG exit then
   rc throw ;

: GRADE-WRITE-DRIVER ( ptr u8 n -- )
   {: a:ptr u:n :}
   SB-RESET
   s" 256 %BLOCK" SB-APPEND 10 SB-APPEND-C
   s" : " SB-APPEND a u SB-APPEND s"  ;" SB-APPEND 10 SB-APPEND-C
   s" CG-RESET CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS 1 SPAN-REG 2 SPAN-REG 1 UNIFORM-REG K CG-RET CG-CLOSE" SB-APPEND 10 SB-APPEND-C
   s" bye" SB-APPEND 10 SB-APPEND-C
   ED-DRIVER$ SB$ WRITE-ALL ;

: GRADE-WRITE-UNCHECKED-DRIVER ( ptr u8 n -- )
   {: a:ptr u:n :}
   SB-RESET
   s" 0 set-check" SB-APPEND 10 SB-APPEND-C
   s" 256 %BLOCK" SB-APPEND 10 SB-APPEND-C
   s" : " SB-APPEND a u SB-APPEND s"  ;" SB-APPEND 10 SB-APPEND-C
   s" CG-RESET CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS 1 SPAN-REG 2 SPAN-REG 1 UNIFORM-REG K CG-RET CG-CLOSE" SB-APPEND 10 SB-APPEND-C
   s" bye" SB-APPEND 10 SB-APPEND-C
   ED-DRIVER$ SB$ WRITE-ALL ;

: GRADE-EMIT ( -- n )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+
   s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f" >LEN PROC-ARGV+
   s" lib/fmt.f" >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f" >LEN PROC-ARGV+
   s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f" >LEN PROC-ARGV+
   s" lib/ptx/tile.f" >LEN PROC-ARGV+
   ED-DRIVER$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN GP-OUT GP-CAP >LEN GP-ERR GQ-CAP >LEN 20000 >MS RUN-ARGV-ENV-CAPTURE
   {: outu:len erru:len rc:rc :}
   ED-PTX$ GP-OUT outu LEN>N WRITE-ALL
   outu LEN>N ;

: GRADE-PTXAS ( -- n )
   ED-PTX$ ED-CUBIN$ GQ-OUT GQ-CAP GQ-ERR GQ-CAP PTX:PTXAS-RUN-DEFAULT
   {: outu:n erru:n rc:n :}
   rc ;

public

: GRADE-CANDIDATE ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u CHECK-PASSES? 0= if 0 exit then
   GRADE-PREPARE
   a u GRADE-WRITE-DRIVER
   GRADE-EMIT 0 = if CLEANUP-RUN 1 exit then
   GRADE-PTXAS 0 <> if CLEANUP-RUN 1 exit then
   ED-CHECKED-VERDICT
   CLEANUP-RUN ;

: GRADE-NOCHECK-CANDIDATE ( ptr u8 n -- n )
   {: a:ptr u:n :}
   GRADE-PREPARE
   a u GRADE-WRITE-UNCHECKED-DRIVER
   GRADE-EMIT 0 = if CLEANUP-RUN EVN-EMIT-FAIL exit then
   GRADE-PTXAS 0 <> if CLEANUP-RUN EVN-PTXAS-FAIL exit then
   ED-NOCHECK-VERDICT
   CLEANUP-RUN ;

variable EVD-PASS
variable EVD-TOTAL

: EVD-RESET ( -- )
   0 EVD-PASS !
   0 EVD-TOTAL ! ;

: EVD-SCORE ( ptr u8 n -- )
   GRADE-CANDIDATE 2 = if EVD-PASS @ 1+ EVD-PASS ! then
   EVD-TOTAL @ 1+ EVD-TOTAL ! ;

end-package
