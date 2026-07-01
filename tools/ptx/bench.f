\ bench.f - generic CUDA Driver kernel benchmark/profile harness.

require lib/errors.f
require lib/string.f
require lib/ffi.f
require tools/ptx/profile.f

package PTXBENCH

128 constant PATH-CAP
128 constant NAME-CAP

create LIB-BUF 16 allot
create SYM-BUF NAME-CAP allot
create PATH-BUF PATH-CAP allot
create KERNEL-BUF NAME-CAP allot
create LABEL-BUF NAME-CAP allot

variable PATH-U
variable KERNEL-U
variable LABEL-U
variable H
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
   {: a u:n dst lenp:ptr :} \ ( a u dst lenp -- ) typed-local-lint: allow-bare-local - ptr roles.
   u PATH-CAP > if E-FS-PATH throw then \ ( -- )
   a dst u BYTE-COPY \ ( -- )
   u lenp ! ; \ ( -- )

: RC0 ( n -- )
   dup 0 <> if s" ptxbench: CUDA Driver call failed" 1 die then \ ( rc -- rc )
   drop ; \ ( rc -- )

: NZ ( n -- )
   dup 0= if s" ptxbench: missing CUDA handle" 1 die then \ ( x -- x )
   drop ; \ ( x -- )

: SYM ( ptr u8 n -- n )
   SYM-BUF >CSTR \ ( a u -- )
   H @ SYM-BUF DLSYM dup NZ ; \ ( -- fn )

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: F32-MS>SCALED ( n n -- n )
   {: b:n scale:n :} \ ( b scale -- )
   b 23 rshift $FF and {: e:n :} \ ( -- )
   e 0= if 0 exit then \ ( -- )
   b $7FFFFF and $800000 or scale * {: m:n :} \ ( -- )
   e 150 - {: sh:n :} \ ( -- )
   sh 0 >= if m sh lshift exit then \ ( -- )
   m 0 sh - rshift ; \ ( -- n )

: F32-MS>US ( n -- n )
   1000 F32-MS>SCALED ; \ ( b -- us )

: F32-MS>NS ( n -- n )
   1000000 F32-MS>SCALED ; \ ( b -- ns )

: EVENTS-CREATE ( -- )
   0 START-EVT ! 0 STOP-EVT ! \ ( -- )
   START-EVT P>N 0 s" cuEventCreate" SYM CALL2 RC0 \ ( -- )
   STOP-EVT P>N 0 s" cuEventCreate" SYM CALL2 RC0 ; \ ( -- )

: EVENTS-DESTROY ( -- )
   START-EVT @ 0 <> if START-EVT @ s" cuEventDestroy_v2" SYM CALL1 RC0 then \ ( -- )
   STOP-EVT @ 0 <> if STOP-EVT @ s" cuEventDestroy_v2" SYM CALL1 RC0 then \ ( -- )
   0 START-EVT ! 0 STOP-EVT ! ; \ ( -- )

: RECORD-START ( -- )
   START-EVT @ 0 s" cuEventRecord" SYM CALL2 RC0 ; \ ( -- )

: RECORD-STOP ( -- )
   STOP-EVT @ 0 s" cuEventRecord" SYM CALL2 RC0 ; \ ( -- )

: STOP-EVENT-SYNC ( -- )
   STOP-EVT @ s" cuEventSynchronize" SYM CALL1 RC0 ; \ ( -- )

: EVENT-ELAPSED-US ( -- n )
   0 EVENT-MS ! \ ( -- )
   EVENT-MS P>N START-EVT @ STOP-EVT @ s" cuEventElapsedTime" SYM CALL3 RC0 \ ( -- )
   EVENT-MS @ $FFFFFFFF and F32-MS>US ; \ ( -- us )

: EVENT-ELAPSED-NS ( -- n )
   0 EVENT-MS ! \ ( -- )
   EVENT-MS P>N START-EVT @ STOP-EVT @ s" cuEventElapsedTime" SYM CALL3 RC0 \ ( -- )
   EVENT-MS @ $FFFFFFFF and F32-MS>NS ; \ ( -- ns )

public

: RESET ( -- )
   0 PATH-U ! \ ( -- )
   0 KERNEL-U ! \ ( -- )
   0 LABEL-U ! \ ( -- )
   0 H ! 0 DEV ! 0 CTX ! 0 MOD ! 0 FUNC ! \ ( -- )
   0 START-EVT ! 0 STOP-EVT ! 0 EVENT-MS ! \ ( -- )
   1 GRID-N ! 256 BLOCK-N ! 1 ITERS-N ! 0 WORK-N ! 0 PARAM-BYTES-N ! ; \ ( -- )

: CUBIN! ( ptr u8 n -- )
   PATH-BUF PATH-U COPY! ; \ ( a u -- )

: KERNEL! ( ptr u8 n -- )
   KERNEL-BUF KERNEL-U COPY! ; \ ( a u -- )

: LABEL! ( ptr u8 n -- )
   LABEL-BUF LABEL-U COPY! ; \ ( a u -- )

: GRID! ( n -- )
   GRID-N ! ; \ ( n -- )

: BLOCK! ( n -- )
   BLOCK-N ! ; \ ( n -- )

: ITERS! ( n -- )
   ITERS-N ! ; \ ( n -- )

: WORK! ( n -- )
   WORK-N ! ; \ ( n -- )

: PARAM-BYTES! ( n -- )
   PARAM-BYTES-N ! ; \ ( n -- )

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
   s" libcuda.so.1" LIB-BUF >CSTR \ ( -- )
   LIB-BUF RTLD-NOW DLOPEN H ! \ ( -- )
   H @ NZ \ ( -- )
   0 DEV ! 0 CTX ! \ ( -- )
   0 s" cuInit" SYM CALL1 RC0 \ ( -- )
   DEV P>N 0 s" cuDeviceGet" SYM CALL2 RC0 \ ( -- )
   CTX P>N DEV @ s" cuDevicePrimaryCtxRetain" SYM CALL2 RC0 \ ( -- )
   CTX @ s" cuCtxSetCurrent" SYM CALL1 RC0 ; \ ( -- )

: LOAD ( -- )
   PATH$ nip 0= if s" ptxbench: cubin path not set" 1 die then \ ( -- )
   KERNEL-U @ 0= if s" ptxbench: kernel not set" 1 die then \ ( -- )
   PATH$ PATH-BUF >CSTR \ ( -- )
   MOD P>N PATH-BUF P>N s" cuModuleLoad" SYM CALL2 RC0 \ ( -- )
   KERNEL-BUF KERNEL-U @ KERNEL-BUF >CSTR \ ( -- )
   FUNC P>N MOD @ KERNEL-BUF P>N s" cuModuleGetFunction" SYM CALL3 RC0 ; \ ( -- )

: UNLOAD ( -- )
   MOD @ 0 <> if MOD @ s" cuModuleUnload" SYM CALL1 RC0 then \ ( -- )
   0 MOD ! 0 FUNC ! ; \ ( -- )

: CLOSE ( -- )
   DEV @ 0 <> if DEV @ s" cuDevicePrimaryCtxRelease" SYM CALL1 RC0 then \ ( -- )
   0 DEV ! 0 CTX ! ; \ ( -- )

: DEVICE-ALLOC ( n ptr a -- )
   {: bytes:n out:ptr :} \ ( bytes out -- )
   out P>N bytes s" cuMemAlloc_v2" SYM CALL2 RC0 ; \ ( -- )

: DEVICE-MEMSET32 ( n n n -- )
   s" cuMemsetD32_v2" SYM CALL3 RC0 ; \ ( dev value count -- )

: DEVICE-FREE ( n -- )
   s" cuMemFree_v2" SYM CALL1 RC0 ; \ ( dev -- )

: HTOD ( n ptr u8 n -- )
   {: dev:n a u:n :} \ ( dev a u -- ) typed-local-lint: allow-bare-local - host ptr u8.
   dev a P>N u s" cuMemcpyHtoD_v2" SYM CALL3 RC0 ; \ ( -- )

: DTOH ( ptr u8 n n -- )
   {: a dev:n u:n :} \ ( a dev u -- ) typed-local-lint: allow-bare-local - host ptr u8.
   a P>N dev u s" cuMemcpyDtoH_v2" SYM CALL3 RC0 ; \ ( -- )

: PARAM! ( n ptr a n -- )
   {: off:n addr:ptr bytes:n :} \ ( off addr bytes -- )
   FUNC @ off addr P>N bytes s" cuParamSetv" SYM CALL4 RC0 ; \ ( -- )

: PARAM-PTR! ( n ptr a -- )
   8 PARAM! ; \ ( off addr -- )

: PARAM-U32! ( n ptr a -- )
   4 PARAM! ; \ ( off addr -- )

: PREPARE-LAUNCH ( -- )
   FUNC @ BLOCK-N @ 1 1 s" cuFuncSetBlockShape" SYM CALL4 RC0 \ ( -- )
   FUNC @ PARAM-BYTES-N @ s" cuParamSetSize" SYM CALL2 RC0 ; \ ( -- )

: LAUNCH ( -- )
   FUNC @ GRID-N @ 1 s" cuLaunchGrid" SYM CALL3 RC0 ; \ ( -- )

: SYNC ( -- )
   0 s" cuCtxSynchronize" SYM CALL1 RC0 ; \ ( -- )

: BENCH-HOST-NS ( -- n )
   LAUNCH SYNC \ ( -- )
   mono-ns {: t0:n :} \ ( -- )
   ITERS-N @ 0 ?do LAUNCH loop \ ( -- )
   SYNC \ ( -- )
   mono-ns t0 - ; \ ( -- ns )

: BENCH-NS ( -- n )
   BENCH-HOST-NS ; \ ( -- ns )

: BENCH-GPU-NS ( -- n )
   EVENTS-CREATE \ ( -- )
   LAUNCH SYNC \ ( -- )
   RECORD-START \ ( -- )
   ITERS-N @ 0 ?do LAUNCH loop \ ( -- )
   RECORD-STOP \ ( -- )
   STOP-EVENT-SYNC \ ( -- )
   EVENT-ELAPSED-NS {: ns:n :} \ ( -- )
   EVENTS-DESTROY \ ( -- )
   ns ; \ ( -- ns )

: REPORT-HEADER ( -- )
   s" kernel=" type LABEL$ type \ ( -- )
   s"  work_items=" type WORK-N @ U.0 \ ( -- )
   s"  grid=" type GRID-N @ U.0 \ ( -- )
   s"  block=" type BLOCK-N @ U.0 \ ( -- )
   s"  iters=" type ITERS-N @ U.0 cr \ ( -- )
   s" param_bytes=" type PARAM-BYTES-N @ U.0 ; \ ( -- )

: REPORT-GPU ( n n n -- )
   {: by:n fl:n ns:n :} \ ( by fl ns -- )
   REPORT-HEADER \ ( -- )
   s"  gpu_elapsed_ns=" type ns U.0 cr \ ( -- )
   by fl ns PTXPROF:REPORT-METRICS ; \ ( -- )

: REPORT-HOST ( n n n -- )
   {: by:n fl:n ns:n :} \ ( by fl ns -- )
   REPORT-HEADER \ ( -- )
   s"  host_elapsed_ns=" type ns U.0 cr \ ( -- )
   by fl ns PTXPROF:REPORT-METRICS ; \ ( -- )

: REPORT ( n n n -- )
   REPORT-GPU ; \ ( by fl ns -- )

end-package
