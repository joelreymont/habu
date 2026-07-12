\ bench.f - generic CUDA Driver kernel benchmark/profile harness.

require lib/errors.f
require lib/string.f
require lib/ffi.f
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
variable GRIDY-N
variable BLOCK-N
variable BLOCKY-N
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
   START-EVT 0 CUDA:CU-EVENT-CREATE CUDA:RC0
   STOP-EVT 0 CUDA:CU-EVENT-CREATE CUDA:RC0 ;

: EVENTS-DESTROY ( -- )
   START-EVT @ 0 <> if START-EVT @ >CUDA-EVENT CUDA:CU-EVENT-DESTROY CUDA:RC0 then
   STOP-EVT @ 0 <> if STOP-EVT @ >CUDA-EVENT CUDA:CU-EVENT-DESTROY CUDA:RC0 then
   0 START-EVT ! 0 STOP-EVT ! ;

: RECORD-START ( -- )
   START-EVT @ >CUDA-EVENT 0 CUDA:CU-EVENT-RECORD CUDA:RC0 ;

: RECORD-STOP ( -- )
   STOP-EVT @ >CUDA-EVENT 0 CUDA:CU-EVENT-RECORD CUDA:RC0 ;

: STOP-EVENT-SYNC ( -- )
   STOP-EVT @ >CUDA-EVENT CUDA:CU-EVENT-SYNCHRONIZE CUDA:RC0 ;

: EVENT-ELAPSED-US ( -- n )
   0 EVENT-MS !
   EVENT-MS START-EVT @ >CUDA-EVENT STOP-EVT @ >CUDA-EVENT CUDA:CU-EVENT-ELAPSED-TIME CUDA:RC0
   EVENT-MS @ $FFFFFFFF and F32-MS>US ;

: EVENT-ELAPSED-NS ( -- n )
   0 EVENT-MS !
   EVENT-MS START-EVT @ >CUDA-EVENT STOP-EVT @ >CUDA-EVENT CUDA:CU-EVENT-ELAPSED-TIME CUDA:RC0
   EVENT-MS @ $FFFFFFFF and F32-MS>NS ;

public

: RESET ( -- )
   0 PATH-U !
   0 KERNEL-U !
   0 LABEL-U !
   0 DEV ! 0 CTX ! 0 MOD ! 0 FUNC !
   0 START-EVT ! 0 STOP-EVT ! 0 EVENT-MS !
   1 GRID-N ! 1 GRIDY-N ! 256 BLOCK-N ! 1 BLOCKY-N ! 1 ITERS-N ! 0 WORK-N ! 0 PARAM-BYTES-N ! ;

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

: GRIDY! ( n -- )       \ 2D grid Y extent (default 1 = 1D grid); the 2D-tiled GEMM needs it
   GRIDY-N ! ;

: BLOCKY! ( n -- )      \ 2D block Y extent (default 1 = 1D block); GEMM uses 16x16 = 256 threads
   BLOCKY-N ! ;

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
   CUDA:OPEN
   0 DEV ! 0 CTX !
   0 CUDA:CU-INIT CUDA:RC0
   DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   CTX DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0 ;

: LOAD ( -- )
   PATH$ nip 0= if s" ptxbench: cubin path not set" 1 die then
   KERNEL-U @ 0= if s" ptxbench: kernel not set" 1 die then
   PATH$ PATH-BUF >CSTR
   MOD PATH-BUF CUDA:CU-MODULE-LOAD CUDA:RC0
   KERNEL-BUF KERNEL-U @ KERNEL-BUF >CSTR
   FUNC MOD @ >CUDA-MOD KERNEL-BUF CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0 ;

: UNLOAD ( -- )
   MOD @ 0 <> if MOD @ >CUDA-MOD CUDA:CU-MODULE-UNLOAD CUDA:RC0 then
   0 MOD ! 0 FUNC ! ;

: CLOSE ( -- )
   DEV @ 0 <> if DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RELEASE CUDA:RC0 then
   0 DEV ! 0 CTX ! ;

: DEVICE-ALLOC ( n ptr a -- )
   {: bytes:n out:ptr :}
   out bytes >LEN CUDA:CU-MEM-ALLOC CUDA:RC0 ;

: DEVICE-MEMSET32 ( n n n -- )
   {: dev:n val:n cnt:n :}
   dev >CUDA-DEVPTR val cnt >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0 ;

: DEVICE-FREE ( n -- )
   >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0 ;

: HTOD ( n ptr u8 n -- )
   {: dev:n a:ptr u:n :}
   dev >CUDA-DEVPTR a u >LEN CUDA:HTOD ;

: DTOH ( ptr u8 n n -- )
   {: a:ptr dev:n u:n :}
   a dev >CUDA-DEVPTR u >LEN CUDA:DTOH ;

: PARAM! ( n ptr a n -- )
   {: off:n addr:ptr bytes:n :}
   FUNC @ >CUDA-FN off >IDX addr bytes >LEN CUDA:CU-PARAM-SET-V CUDA:RC0 ;

: PARAM-PTR! ( n ptr a -- )
   8 PARAM! ;

: PARAM-U32! ( n ptr a -- )
   4 PARAM! ;

: PREPARE-LAUNCH ( -- )
   FUNC @ >CUDA-FN BLOCK-N @ BLOCKY-N @ 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   FUNC @ >CUDA-FN PARAM-BYTES-N @ >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0 ;

: LAUNCH ( -- )
   FUNC @ >CUDA-FN GRID-N @ GRIDY-N @ CUDA:CU-LAUNCH-GRID CUDA:RC0 ;

: SYNC ( -- )
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0 ;

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
   s"  work_items=" type WORK-N @ .U
   s"  grid=" type GRID-N @ .U
   s"  block=" type BLOCK-N @ .U
   s"  iters=" type ITERS-N @ .U cr
   s" param_bytes=" type PARAM-BYTES-N @ .U ;

: REPORT-GPU ( n n n -- )
   {: by:n fl:n ns:n :}
   REPORT-HEADER
   s"  gpu_elapsed_ns=" type ns .U cr
   by fl ns PTXPROF:REPORT-METRICS ;

: REPORT-HOST ( n n n -- )
   {: by:n fl:n ns:n :}
   REPORT-HEADER
   s"  host_elapsed_ns=" type ns .U cr
   by fl ns PTXPROF:REPORT-METRICS ;

: REPORT ( n n n -- )
   REPORT-GPU ;

;package
