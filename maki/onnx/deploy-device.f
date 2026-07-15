\ maki/onnx/deploy-device.f - the ONNX-imported-model WHOLE-MODEL device golden driver.
\
\ Closes dot habu-maki-onnx-import's device leg: a committed ONNX ModelProto is taken
\ ONNX:IMPORT -> the maki model IR -> FP-BUILD (OPTIMIZE's fusion regions) -> the whole-model
\ device harness (maki/lower-launch.f LOWER-MODEL-RUN, region-by-region on device with cross-
\ region f32 buffers), and the FINAL device output is compared against the COMMITTED onnxruntime
\ reference values (maki/onnx/ort-ref-data.f ORF-Y) - not just device-vs-host.
\
\ The fixture is the ort-ref MLP (Gemm(x,w1,b1) -> Relu -> Gemm(a,w2,b2), the LINEAR RELU LINEAR
\ shape the whole-model device path supports): it fuses into 2 MATMUL regions (region 0 = matmul +
\ relu epilogue, region 1 = matmul reading region 0's device buffer). maki/onnx/ort-ref-test.f
\ already proves ONNX:IMPORT + the HOST executor reproduce onnxruntime within 1e-5 on the committed
\ input; this file adds the DEVICE leg on the SAME committed input.
\
\ Shared-arena input binding. The device path reads each region input from the golden arena
\ (lower-launch LLA-PACK-INPUT -> GA-IN-PTR), and GA-BIND-SYNTH binds the SAME arena buffers to the
\ host executor (EX). So ODV-BIND-REAL runs GA-BIND-SYNTH to allocate+bind the slots, then
\ OVERWRITES each slot's arena buffer with the real ORF data (the committed input x + the imported
\ initializers). Host and device then execute on identical committed values - the host leg is free
\ off-device coverage, the device leg is the golden.
\
\ Tolerance (device f32 vs onnxruntime f32). |device - ort| <= |device - host_f32| + |host_f32 - ort|.
\ The first term is the lower-golden composed device-vs-host bound (maki/lower-golden.f MDL-ATOL/
\ MDL-RTOL: summed per-region matmul atol/rtol - the accumulate-across-regions discipline). The
\ second is the host-f64-vs-ort-f32 rounding floor the ort-ref test measured at 1.24e-7 and licenses
\ at ORF-TOL 1e-5 (ODV-ORT-FLOOR here). So the device bound is (MDL-ATOL + ODV-ORT-FLOOR, MDL-RTOL),
\ stated honestly: composed device rounding plus the ort dtype floor.
\
\ Corruption sensitivity - that a WRONG kernel/import is REJECTED (not silently passed) is inherited
\ from the shared comparator's proof (maki/ablate-golden-device-test.f) and demonstrated for this
\ fixture by a temp-copy PTX perturbation on the Orin (documented in the test header, not committed).
\
\ Off the Orin (no libcuda) the device leg is SKIPPED (CUDA:OPEN? probe) and the host leg still runs,
\ so the file check-loads + proves the host halves where there is no CUDA. Defining words only (no
\ load-time run) - the spawned emit child re-requires this file to rebuild the imported IR. Fully
\ checked Habu; maki -> habu only. Not part of maki/test.f (needs the CUDA toolkit + a device).

require lib/test.f
require lib/float.f
require lib/fmt.f
require maki/onnx/import.f
require maki/onnx/ort-ref-data.f
require maki/lower-model-device.f

-5239 constant E-ODV-INPUTS   \ the committed ORF fixture must have exactly one runtime input (x)

\ ---- public fixture bridges: the ORF golden data is ONNX-ORT-TEST-private, so expose the exact
\ spans the device driver (and the spawned emit child) need without weakening the committed fixture.
package ONNX-ORT-TEST public
: ODV-MODEL$ ( -- ptr u8 n )  ORF-REF$ ;   \ the committed ModelProto bytes (child re-imports these)
: ODV-X-PTR  ( -- ptr a )     ORF-X ;      \ committed input tensor (row-major cells)
: ODV-Y-PTR  ( -- ptr a )     ORF-Y ;      \ onnxruntime reference output (f32 widened to f64)
: ODV-YN     ( -- n )         ORF-YN ;      \ reference output element count
;package

package MAKI

\ ---- device-vs-onnxruntime tolerance ----------------------------------------
: ODV-ORT-FLOOR ( -- r )  0.00001 ;        \ host-f64-vs-ort-f32 floor (ort-ref-test ORF-TOL, 1e-5)
: ODV-DEV-ATOL  ( -- r )  MDL-ATOL ODV-ORT-FLOOR f+ ;   \ composed device-vs-host atol + ort floor
: ODV-DEV-RTOL  ( -- r )  MDL-RTOL ;                    \ composed device-vs-host rtol
: ODV-WITHIN? ( r r r r -- bool ) {: dev:r ref:r atol:r rtol:r :}   \ |dev-ref| <= atol + rtol*|ref|
   atol  rtol ref fabs f* f+ {: tol:r :}
   dev ref f- fabs {: d:r :}
   tol d f< 0= ;
: ODV-CLOSE? ( r r -- bool )  f- fabs ODV-ORT-FLOOR f< ;   \ host-vs-ort within the ort floor
: ODV-Y@ ( n -- r )  ONNX-ORT-TEST:ODV-Y-PTR swap T-GET ;  \ onnxruntime reference element n

\ ---- shared-arena input binding: real committed data into both legs' input buffers ----
: ODV-CP ( ptr a ptr a n -- ) {: s:ptr d:ptr n:n :}  n 0 ?do  s i T-GET  d i T-SET  loop ;
: ODV-BIND-REAL ( -- )
   GA-BIND-SYNTH                                    \ allocate + bind every slot's arena buffer (host+device)
   ONNX:IN# 1 <> if E-ODV-INPUTS throw then         \ the ORF fixture is single-input (x)
   ONNX-ORT-TEST:ODV-X-PTR  0 ONNX:IN-SLOT@ dup GA-IN-PTR swap GA-SLOT-ELEMS ODV-CP
   ONNX:INIT# 0 ?do
      i ONNX:INIT-DATA@  i ONNX:INIT-SLOT@ dup GA-IN-PTR swap GA-SLOT-ELEMS ODV-CP
   loop ;

\ ---- host leg (off-device free coverage): host executor on the committed input == ort golden ----
: ODV-HOST@ ( n -- r )  ONNX:OUT-NODE@ EX-OUT@ swap T-GET ;
: ODV-HOST-CHECK ( -- )
   MIR-N@ EX-RUN-N
   ONNX-ORT-TEST:ODV-YN 0 ?do
      i ODV-HOST@  i ODV-Y@  ODV-CLOSE? TTRUE
   loop ;

\ ---- per-region cubin build: spawn a fresh bin/hb that RE-IMPORTS the committed bytes + emits ----
\ mirrors maki/lower-model-device.f LMDM-EMIT, but the child rebuilds the IR by ONNX:IMPORT of the
\ committed bytes (reqa pulls this file -> the ONNX importer + fixture + every REGION emit word).
: ODV-EMIT ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   s" ONNX-ORT-TEST:ODV-MODEL$ ONNX:IMPORT"        \ ma: the child re-imports the committed ModelProto
   s" require maki/onnx/deploy-device.f"           \ reqa: the ONNX importer + fixture + emit words
   rid LMDM-EMIT$                                  \ emita: the region-class REGION emit word
   rid  MAKI-GRADE:DRIVER$  LOWER-DRIVER!
   PROC-ARGV-RESET
   s" --load"           >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$   >LEN PROC-ARGV+
   s" bin/hb" >LEN  LMDM-OUT $4000 >LEN  LMDM-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   LMDM-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD  \ surface child stderr + throw on nonzero exit
   PTXTC:PTX$ LMDM-OUT outu LEN>N WRITE-ALL ;

: ODV-ASSEMBLE-REGION ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid ODV-EMIT
   LMDM-PTXAS PTXTC:ASM-REPORT 0 T=                \ ptxas rc 0 (stderr surfaced on failure)
   PTXTC:CUBIN$ LMDM-CBUF $20000 READ-ALL {: got:n :}
   rid LMDM-RP$ {: pa:ptr pu:n :}
   pa pu LMDM-CBUF got WRITE-ALL
   pa pu rid MDL-CUBIN! ;

: ODV-BUILD-CUBINS ( -- )
   MDL-CUBINS-RESET
   FP-REGION-COUNT 0 ?do  i FP-REGION-ID ODV-ASSEMBLE-REGION  loop ;

\ ---- per-element evidence (final device output | onnxruntime reference) ----
: ODV-FP ( r -- )  SB-RESET 6 SB-FIX SB$ type ;
: ODV-EVIDENCE ( -- )
   s" elem   device        ort_f32" type cr
   ONNX-ORT-TEST:ODV-YN 0 ?do
      s"   " type i SB-RESET SB-INT SB$ type s"    " type
      i LLA-OUT@ ODV-FP  s"    " type  i ODV-Y@ ODV-FP  cr
   loop ;

\ ---- the device golden: build every region's cubin, run whole-model on device, compare vs ort ----
\ Skips (no emit/ptxas/run) off-device, so the file still loads where there is no CUDA.
: ODV-DEVICE-GOLD ( -- )
   CUDA:OPEN? 0= if exit then
   ODV-BUILD-CUBINS
   LOWER-MODEL-RUN                                  \ device: region by region, final output -> LLA-HOUT
   LLA-ELEMS@ ONNX-ORT-TEST:ODV-YN T=              \ device output element count == reference count
   MDL-COUNT-REGIONS                               \ tally regions for the composed tolerance
   s" lower-onnx-device-golden: device vs onnxruntime (" type
      MDL-N-REGIONS@ SB-RESET SB-INT SB$ type s"  regions)" type cr
   ODV-EVIDENCE
   ODV-DEV-ATOL {: atol:r :}  ODV-DEV-RTOL {: rtol:r :}
   ONNX-ORT-TEST:ODV-YN 0 ?do
      i LLA-OUT@  i ODV-Y@  atol rtol ODV-WITHIN? TTRUE
   loop ;

;package
