\ bench.f - generic CUDA Driver kernel benchmark/profile harness.

require lib/errors.f
require lib/string.f
require lib/ptx/cuda-driver.f
require tools/ptx/profile.f

package PTXBENCH

128 constant PATH-CAP
128 constant NAME-CAP

create PATH-BUF PATH-CAP allot
create KERNEL-BUF NAME-CAP allot
create LABEL-BUF NAME-CAP allot

variable PATH-U
variable KERNEL-U
variable LABEL-U
variable DEV
variable CTX
variable MOD
variable FUNC
variable START-EVT
variable STOP-EVT
variable EVENT-MS
variable GRID-N
variable BLOCK-N
variable ITERS-N
variable WORK-N
variable PARAM-BYTES-N

: COPY! ( ptr u8 n ptr u8 ptr a -- )
   {: a u:n dst lenp:ptr :} \ typed-local-lint: allow-bare-local - ptr roles.
   u PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: F32-MS>SCALED ( n n -- n )
   {: b:n scale:n :}
   b 23 rshift $FF and {: e:n :}
   e 0= if 0 exit then
   b $7FFFFF and $800000 or scale * {: m:n :}
   e 150 - {: sh:n :}
   sh 0 >= if m sh lshift exit then
   m 0 sh - rshift ;

: F32-MS>US ( n -- n )
   1000 F32-MS>SCALED ;

: F32-MS>NS ( n -- n )
   1000000 F32-MS>SCALED ;

: EVENTS-CREATE ( -- )
   0 START-EVT ! 0 STOP-EVT !
   START-EVT P>N 0 s" cuEventCreate" CUDA:CALL2-RC
   STOP-EVT P>N 0 s" cuEventCreate" CUDA:CALL2-RC ;

: EVENTS-DESTROY ( -- )
   START-EVT @ 0 <> if START-EVT @ s" cuEventDestroy_v2" CUDA:CALL1-RC then
   STOP-EVT @ 0 <> if STOP-EVT @ s" cuEventDestroy_v2" CUDA:CALL1-RC then
   0 START-EVT ! 0 STOP-EVT ! ;

: RECORD-START ( -- )
   START-EVT @ 0 s" cuEventRecord" CUDA:CALL2-RC ;

: RECORD-STOP ( -- )
   STOP-EVT @ 0 s" cuEventRecord" CUDA:CALL2-RC ;

: STOP-EVENT-SYNC ( -- )
   STOP-EVT @ s" cuEventSynchronize" CUDA:CALL1-RC ;

: EVENT-ELAPSED-US ( -- n )
   0 EVENT-MS !
   EVENT-MS P>N START-EVT @ STOP-EVT @ s" cuEventElapsedTime" CUDA:CALL3-RC
   EVENT-MS @ $FFFFFFFF and F32-MS>US ;

: EVENT-ELAPSED-NS ( -- n )
   0 EVENT-MS !
   EVENT-MS P>N START-EVT @ STOP-EVT @ s" cuEventElapsedTime" CUDA:CALL3-RC
   EVENT-MS @ $FFFFFFFF and F32-MS>NS ;

public

: RESET ( -- )
   0 PATH-U !
   0 KERNEL-U !
   0 LABEL-U !
   0 DEV ! 0 CTX ! 0 MOD ! 0 FUNC !
   0 START-EVT ! 0 STOP-EVT ! 0 EVENT-MS !
   1 GRID-N ! 256 BLOCK-N ! 1 ITERS-N ! 0 WORK-N ! 0 PARAM-BYTES-N ! ;

: CUBIN! ( ptr u8 n -- )
   PATH-BUF PATH-U COPY! ;

: KERNEL! ( ptr u8 n -- )
   KERNEL-BUF KERNEL-U COPY! ;

: LABEL! ( ptr u8 n -- )
   LABEL-BUF LABEL-U COPY! ;

: GRID! ( n -- )
   GRID-N ! ;

: BLOCK! ( n -- )
   BLOCK-N ! ;

: ITERS! ( n -- )
   ITERS-N ! ;

: WORK! ( n -- )
   WORK-N ! ;

: PARAM-BYTES! ( n -- )
   PARAM-BYTES-N ! ;

: GRID@ ( -- n )
   GRID-N @ ;

: BLOCK@ ( -- n )
   BLOCK-N @ ;

: ITERS@ ( -- n )
   ITERS-N @ ;

: WORK@ ( -- n )
   WORK-N @ ;

: PARAM-BYTES@ ( -- n )
   PARAM-BYTES-N @ ;

: LABEL$ ( -- ptr u8 n )
   LABEL-BUF LABEL-U @ ;

: OPEN ( -- )
   CUDA:RESET
   0 DEV ! 0 CTX !
   CUDA:INIT
   DEV CUDA:DEVICE-GET
   CTX DEV @ CUDA:PRIMARY-CTX-RETAIN
   CTX @ CUDA:CTX-CURRENT! ;

: LOAD ( -- )
   PATH$ nip 0= if E-PTX-CUDA-CUBIN throw then
   KERNEL-U @ 0= if E-PTX-CUDA-DLSYM throw then
   PATH$ MOD CUDA:LOAD-MODULE
   MOD @ KERNEL-BUF KERNEL-U @ FUNC CUDA:MODULE-FUNCTION ;

: UNLOAD ( -- )
   MOD @ CUDA:UNLOAD-MODULE
   0 MOD ! 0 FUNC ! ;

: CLOSE ( -- )
   CTX @ 0 <> if DEV @ CUDA:PRIMARY-CTX-RELEASE then
   0 DEV ! 0 CTX ! ;

: DEVICE-ALLOC ( n ptr a -- )
   CUDA:DEVICE-ALLOC ;

: DEVICE-MEMSET32 ( n n n -- )
   CUDA:MEMSET32 ;

: DEVICE-FREE ( n -- )
   CUDA:DEVICE-FREE ;

: HTOD ( n ptr u8 n -- )
   CUDA:HTOD ;

: DTOH ( ptr u8 n n -- )
   CUDA:DTOH ;

: PARAM! ( n ptr a n -- )
   {: off:n addr:ptr bytes:n :}
   FUNC @ off addr bytes CUDA:PARAM! ;

: PARAM-PTR! ( n ptr a -- )
   8 PARAM! ;

: PARAM-U32! ( n ptr a -- )
   4 PARAM! ;

: PREPARE-LAUNCH ( -- )
   FUNC @ BLOCK-N @ 1 1 CUDA:BLOCK-SHAPE
   FUNC @ PARAM-BYTES-N @ CUDA:PARAM-SIZE ;

: LAUNCH ( -- )
   FUNC @ GRID-N @ 1 CUDA:LAUNCH-GRID ;

: SYNC ( -- )
   CUDA:SYNC ;

: BENCH-HOST-NS ( -- n )
   LAUNCH SYNC
   mono-ns {: t0:n :}
   ITERS-N @ 0 ?do LAUNCH loop
   SYNC
   mono-ns t0 - ;

: BENCH-NS ( -- n )
   BENCH-HOST-NS ;

: BENCH-GPU-NS ( -- n )
   EVENTS-CREATE
   LAUNCH SYNC
   RECORD-START
   ITERS-N @ 0 ?do LAUNCH loop
   RECORD-STOP
   STOP-EVENT-SYNC
   EVENT-ELAPSED-NS {: ns:n :}
   EVENTS-DESTROY
   ns ;

: REPORT-HEADER ( -- )
   s" kernel=" type LABEL$ type
   s"  work_items=" type WORK-N @ U.0
   s"  grid=" type GRID-N @ U.0
   s"  block=" type BLOCK-N @ U.0
   s"  iters=" type ITERS-N @ U.0 cr
   s" param_bytes=" type PARAM-BYTES-N @ U.0 ;

: REPORT-GPU ( n n n -- )
   {: by:n fl:n ns:n :}
   REPORT-HEADER
   s"  gpu_elapsed_ns=" type ns U.0 cr
   by fl ns PTXPROF:REPORT-METRICS ;

: REPORT-HOST ( n n n -- )
   {: by:n fl:n ns:n :}
   REPORT-HEADER
   s"  host_elapsed_ns=" type ns U.0 cr
   by fl ns PTXPROF:REPORT-METRICS ;

: REPORT ( n n n -- )
   REPORT-GPU ;

end-package
