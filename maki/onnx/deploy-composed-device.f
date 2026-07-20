\ maki/onnx/deploy-composed-device.f - the COMPOSED-Gemm whole-model device-golden driver.
\
\ Closes dot habu-device-golden-composed's device leg: the committed composed-Gemm ModelProto
\ (maki/onnx/composed-ref-data.f, a transB=1 Gemm MLP) is taken ONNX:IMPORT -> the maki model IR
\ -> FP-BUILD (fusion regions) -> the whole-model device harness (maki/lower/launch.f
\ LOWER-MODEL-RUN, region-by-region on device with cross-region f32 buffers), and the FINAL device
\ output is compared against the committed HOST-ORACLE reference (composed-ref-data.f CRF-Y).
\
\ WHAT IS NEW vs deploy-device.f (the default-affine ort-ref golden): the composed graph carries
\ an inserted TRANSPOSE MOVEMENT node (import.f IMP-GEMM-COMPOSED). FP-BUILD makes that transpose a
\ STANDALONE MATERIALIZED movement region (region 0) whose device output buffer is READ BY the
\ matmul region (region 1). So this golden exercises the whole-model device path with a MOVEMENT
\ region feeding a MATMUL region - the class dispatch (maki/lower/model-device.f LMDM-EMIT$ ->
\ LMV-EMIT for the copy kernel, LMM-EMIT for the matmuls) and the movement->matmul cross-region
\ device buffer that the pure-matmul ort-ref golden never covered. The movement region class is
\ device-emittable: maki/lower/mv-device-test.f already goldens a standalone materialized TRANSPOSE
\ copy kernel, and LOWER-MODEL-RUN routes region 0 through LMV-RUN.
\
\ REFERENCE ORACLE (honest): CRF-Y is the maki HOST executor's output, NOT onnxruntime - no ort is
\ available for a composed Gemm and the repo has no committed composed-Gemm ort reference. The host
\ executor is the validated oracle (== onnxruntime within 1e-5 on the ort-ref fixture,
\ maki/onnx/ort-ref-test.f). So this is the device-vs-host discipline with a committed host oracle;
\ the missing ort leg is a documented residual (see composed-ref-data.f).
\
\ Shared-arena input binding (as in deploy-device.f). The device path reads each region input from
\ the golden arena (GA-IN-PTR); CDV-BIND-REAL runs GA-BIND-SYNTH to allocate+bind every slot, then
\ OVERWRITES each slot's arena buffer with the REAL committed data (input x + the imported weight
\ initializers, including the w1t the transpose region transposes on device). Host and device then
\ execute on identical committed values - the host leg is free off-device coverage, the device leg
\ is the golden.
\
\ Tolerance (device f32 vs the host-f64 oracle) = the composed device-vs-host bound MDL-ATOL/MDL-RTOL
\ (maki/lower/golden.f), which SUMS each region class's precision row: this model is 2 MATMUL + 1
\ MOVEMENT region, so atol = 2*mm + 1*mv (3e-6) and rtol = 2*mm + 1*mv (2.0001e-4); the MOVEMENT
\ region's contribution is its NUM-EXACT copy row (1e-6/1e-6), stated honestly. No ort dtype floor
\ is added because the reference IS the host executor (not an f32 ort run).
\
\ Off the Orin (no libcuda) the device leg is SKIPPED (CUDA:OPEN? probe) and the host leg still runs,
\ so the file check-loads + proves the host halves where there is no CUDA. Defining words only (no
\ load-time run): the spawned emit child re-requires THIS file to rebuild the imported IR + every
\ region emit word. Fully checked Habu; maki -> habu only. Not part of maki/test.f (needs the CUDA
\ toolkit + a device). Reuses the maki/lower/model-device.f LMDM-* toolchain buffers/helpers.

require lib/test.f
require lib/float.f
require lib/fmt.f
require maki/onnx/import.f
require maki/onnx/composed-ref-data.f
require maki/lower/model-device.f

-5273 constant E-CDV-INPUTS   \ the committed composed fixture must have exactly one runtime input (x)

\ ---- public fixture bridges: the CRF golden data is ONNX-CMP-TEST-private, so expose the exact
\ spans the device driver (and the spawned emit child) need without weakening the committed fixture.
package ONNX-CMP-TEST public
: CDV-MODEL$ ( -- ptr u8 n )  CRF-MODEL$ ;   \ the composed ModelProto bytes (child re-imports these)
: CDV-X-PTR  ( -- ptr a )     CRF-X ;        \ committed input tensor (row-major cells)
: CDV-Y-PTR  ( -- ptr a )     CRF-Y ;        \ host-oracle reference output
: CDV-YN     ( -- n )         CRF-YN ;        \ reference output element count
;package

package MAKI

\ ---- device-vs-host-oracle tolerance (the composed device-vs-host bound) ----------------------
: CDV-DEV-ATOL  ( -- r )  MDL-ATOL ;               \ composed device-vs-host atol (sums each region class)
: CDV-DEV-RTOL  ( -- r )  MDL-RTOL ;               \ composed device-vs-host rtol
: CDV-HOST-FLOOR ( -- r )  0.000000001 ;           \ host==committed self-consistency floor (values exact)
: CDV-WITHIN? ( r r r r -- bool ) {: dev:r ref:r atol:r rtol:r :}   \ |dev-ref| <= atol + rtol*|ref|
   atol  rtol ref fabs f* f+ {: tol:r :}
   dev ref f- fabs {: d:r :}
   tol d f< 0= ;
: CDV-CLOSE? ( r r -- bool )  f- fabs CDV-HOST-FLOOR f< ;   \ host == committed oracle within the floor
: CDV-Y@ ( n -- r )  ONNX-CMP-TEST:CDV-Y-PTR swap T-GET ;   \ host-oracle reference element n

\ ---- shared-arena input binding: real committed data into both legs' input buffers ----
: CDV-CP ( ptr a ptr a n -- ) {: s:ptr d:ptr n:n :}  n 0 ?do  s i T-GET  d i T-SET  loop ;
: CDV-BIND-REAL ( -- )
   GA-BIND-SYNTH                                    \ allocate + bind every slot's arena buffer (host+device)
   ONNX:IN# 1 <> if E-CDV-INPUTS throw then         \ the composed fixture is single-input (x)
   ONNX-CMP-TEST:CDV-X-PTR  0 ONNX:IN-SLOT@ dup GA-IN-PTR swap GA-SLOT-ELEMS CDV-CP
   ONNX:INIT# 0 ?do
      i ONNX:INIT-DATA@  i ONNX:INIT-SLOT@ dup GA-IN-PTR swap GA-SLOT-ELEMS CDV-CP
   loop ;

\ ---- host leg (off-device free coverage): host executor on the committed input == CRF-Y oracle ----
: CDV-HOST@ ( n -- r )  ONNX:OUT-NODE@ EX-OUT@ swap T-GET ;
: CDV-HOST-CHECK ( -- )
   MIR-N@ EX-RUN-N
   ONNX-CMP-TEST:CDV-YN 0 ?do
      i CDV-HOST@  i CDV-Y@  CDV-CLOSE? TTRUE
   loop ;

\ ---- per-region cubin build: spawn a fresh bin/hb that RE-IMPORTS the committed bytes + emits ----
\ mirrors maki/lower/model-device.f LMDM-EMIT, but the child rebuilds the IR by ONNX:IMPORT of the
\ committed bytes (reqa pulls this file -> the ONNX importer + fixture + every REGION emit word,
\ including LMV-EMIT for the movement region).
: CDV-EMIT ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   s" ONNX-CMP-TEST:CDV-MODEL$ ONNX:IMPORT"         \ ma: the child re-imports the committed ModelProto
   s" require maki/onnx/deploy-composed-device.f"   \ reqa: the ONNX importer + fixture + emit words
   rid LMDM-EMIT$                                   \ emita: the region-class REGION emit word
   rid  MAKI-GRADE:DRIVER$  LOWER-DRIVER!
   PROC-ARGV-RESET
   s" --load"           >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$   >LEN PROC-ARGV+
   s" bin/hb" >LEN  LMDM-OUT $4000 >LEN  LMDM-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC ENDOF          \ clean exit -> rc 0
     err OF PCAP-FAILED:UNMAKE ENDOF                  \ nonzero child: (out err code) on stack
   ;MATCH
   {: outu:len erru:len rc:rc :}
   LMDM-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD   \ surface child stderr + throw on nonzero exit
   PTXTC:PTX$ LMDM-OUT outu LEN>N WRITE-ALL ;

: CDV-ASSEMBLE-REGION ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid CDV-EMIT
   LMDM-PTXAS PTXTC:ASM-REPORT 0 T=                 \ ptxas rc 0 (stderr surfaced on failure)
   PTXTC:CUBIN$ LMDM-CBUF $20000 READ-ALL {: got:n :}
   rid LMDM-RP$ {: pa:ptr pu:n :}
   pa pu LMDM-CBUF got WRITE-ALL
   pa pu rid MDL-CUBIN! ;

: CDV-BUILD-CUBINS ( -- )
   MDL-CUBINS-RESET
   FP-REGION-COUNT 0 ?do  i FP-REGION-ID CDV-ASSEMBLE-REGION  loop ;

\ ---- per-element evidence (final device output | host-oracle reference) ----
: CDV-FP ( r -- )  SB-RESET 6 FMT:SB-FIX SB$ type ;
: CDV-EVIDENCE ( -- )
   s" elem   device        host_oracle" type cr
   ONNX-CMP-TEST:CDV-YN 0 ?do
      s"   " type i SB-RESET FMT:SB-INT SB$ type s"    " type
      i LLA-OUT@ CDV-FP  s"    " type  i CDV-Y@ CDV-FP  cr
   loop ;

\ ---- the device golden: build every region's cubin, run whole-model on device, compare vs oracle --
\ Skips (no emit/ptxas/run) off-device, so the file still loads where there is no CUDA.
: CDV-DEVICE-GOLD ( -- )
   CUDA:OPEN? 0= if exit then
   CDV-BUILD-CUBINS
   LOWER-MODEL-RUN                                  \ device: region by region, final output -> LLA-HOUT
   LLA-ELEMS@ ONNX-CMP-TEST:CDV-YN T=              \ device output element count == reference count
   MDL-COUNT-REGIONS                               \ tally regions for the composed tolerance
   s" deploy-composed-device: device vs host oracle (" type
      MDL-N-REGIONS@ SB-RESET FMT:SB-INT SB$ type s"  regions: " type
      MDL-N-MV@ SB-RESET FMT:SB-INT SB$ type s"  move + " type
      MDL-N-MM@ SB-RESET FMT:SB-INT SB$ type s"  matmul)" type cr
   CDV-EVIDENCE
   CDV-DEV-ATOL {: atol:r :}  CDV-DEV-RTOL {: rtol:r :}
   ONNX-CMP-TEST:CDV-YN 0 ?do
      i LLA-OUT@  i CDV-Y@  atol rtol CDV-WITHIN? TTRUE
   loop ;

;package
