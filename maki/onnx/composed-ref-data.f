\ maki/onnx/composed-ref-data.f - committed fixture for the COMPOSED-Gemm device golden
\ (dot habu-device-golden-composed): a transB=1 Gemm MLP that lowers by COMPOSITION - an
\ inserted TRANSPOSE movement node around the matmul (maki/onnx/import.f IMP-GEMM-COMPOSED).
\
\ Model (2-layer, bias-free): Gemm(x, w1t, transB=1) -> Relu -> Gemm(a, w2).
\   x 2x4 (runtime input), w1t 8x4 initializer (transB weight; w1t^T is 4x8), h 2x8,
\   a = Relu(h) 2x8, w2 8x2 initializer, y 2x2. The transB=1 makes layer 1 the COMPOSED
\   form: import inserts TRANSPOSE(w1t) -> w1t^T then MATMUL(x, w1t^T), so the graph carries
\   a standalone materialized MOVEMENT region feeding the matmul region - the whole-model
\   device path the default-affine ort-ref fixture (maki/onnx/deploy-device-test.f) never
\   exercises. Layer 2 is a plain 2-input Gemm (bare matmul). No separate bias/alpha node:
\   OP-BIAS / OP-SCALE are NOT v1 matmul epilogue ops (maki/lower-mm.f LMM-EPI-OP? accepts
\   only relu/gelu/silu), so a composed Gemm with a separate bias or alpha<>1 fails closed on
\   the device matmul route (E-LMM-OP). CRF-ALPHA-MODEL$ carries that NEGATIVE fixture and the
\   sibling test proves the fail-closed rejection - the documented alpha/bias residual.
\
\ NO BINARY BLOB: CRF-MODEL$ builds the ModelProto through the maki/onnx/encode.f DSL (the
\ same encoder ort-ref-data.f re-encodes onnx's export with), so the fixture is source-defined
\ and the spawned emit child rebuilds identical bytes deterministically.
\
\ REFERENCE PROVENANCE (honest): there is NO onnxruntime available for a composed Gemm, and
\ the repo has no committed composed-Gemm ort reference to reuse. CRF-Y is therefore the HOST
\ EXECUTOR oracle (maki/executor.f EX-RUN) - the SAME host executor proven equal to onnxruntime
\ within 1e-5 on the default-affine ort-ref fixture (maki/onnx/ort-ref-test.f). So the composed
\ golden is HOST-ORACLE-based (device-vs-host discipline), NOT ort-based; the missing ort leg
\ for the composed form is a documented residual. All weights/inputs are 2^-2-grid dyadic
\ fractions (exact in f32 and f64) with tiny K, so every matmul accumulation is exact in f32:
\ CRF-Y are the exact rational outputs, and device f32 == host f64 to the bit within the
\ composed tolerance. maki -> habu only; the composed device golden owns -5273..-5274.

require lib/prelude.f
require lib/string.f
require lib/float.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require maki/onnx/encode.f
require maki/onnx/proto.f
require maki/array.f

package ONNX-CMP-TEST

\ ---- shapes (the fixture's committed dimensions) ------------------------------
2 constant CRF-BATCH
4 constant CRF-IN
8 constant CRF-HID
2 constant CRF-OUT
8 constant CRF-XN           \ CRF-BATCH * CRF-IN
4 constant CRF-YN           \ CRF-BATCH * CRF-OUT

\ ---- encode helpers (fixture builder; mirror ort-ref-data.f's private DSL) -----
: CRF-DIM+ ( n -- ) {: d:n :}                  \ one TensorShapeProto.Dimension
   1 ONNX:ENC-SUB  d 1 ONNX:ENC-INT  ONNX:;ENC-SUB ;

: CRF-VI+ ( ptr u8 n n n n -- ) {: a:ptr u:n fld:n rows:n cols:n :}   \ 2D f32 ValueInfo
   fld ONNX:ENC-SUB
      a u 1 ONNX:ENC-STR
      2 ONNX:ENC-SUB  1 ONNX:ENC-SUB
         1 1 ONNX:ENC-INT
         2 ONNX:ENC-SUB  rows CRF-DIM+  cols CRF-DIM+  ONNX:;ENC-SUB
      ONNX:;ENC-SUB  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

\ open a 2D f32 initializer (dims r c, raw_data payload follows; )CRF-INIT closes)
: CRF-INIT2( ( ptr u8 n n n -- ) {: a:ptr u:n r:n c:n :}
   5 ONNX:ENC-SUB
      r 1 ONNX:ENC-INT  c 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  a u 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB ;
: )CRF-INIT ( -- )  ONNX:;ENC-SUB  ONNX:;ENC-SUB ;

\ opset_import { domain:"" version:13 }
: CRF-OPSET+ ( -- )
   8 ONNX:ENC-SUB
      1 ONNX:WT-LEN ONNX:ENC-TAG  0 ONNX:ENC-VARINT
      13 2 ONNX:ENC-INT
   ONNX:;ENC-SUB ;

: CRF-GEMM-TB+ ( ptr u8 n ptr u8 n ptr u8 n -- )   \ Gemm x w -> out, transB=1 (2-input, composed)
   {: xa:ptr xu:n wa:ptr wu:n oa:ptr ou:n :}
   1 ONNX:ENC-SUB
      xa xu 1 ONNX:ENC-STR  wa wu 1 ONNX:ENC-STR  oa ou 2 ONNX:ENC-STR
      s" Gemm" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" transB" 1 ONNX:ENC-STR  1 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: CRF-GEMM+ ( ptr u8 n ptr u8 n ptr u8 n -- )   \ Gemm x w -> out (2-input, default affine)
   {: xa:ptr xu:n wa:ptr wu:n oa:ptr ou:n :}
   1 ONNX:ENC-SUB
      xa xu 1 ONNX:ENC-STR  wa wu 1 ONNX:ENC-STR  oa ou 2 ONNX:ENC-STR
      s" Gemm" 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: CRF-RELU+ ( ptr u8 n ptr u8 n -- ) {: ia:ptr iu:n oa:ptr ou:n :}   \ Relu in -> out
   1 ONNX:ENC-SUB
      ia iu 1 ONNX:ENC-STR  oa ou 2 ONNX:ENC-STR  s" Relu" 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: CRF-W1-DATA ( -- )                           \ w1t 8x4 row-major raw_data (transB weight)
   0.5 ONNX:ENC-F32     0.0 ONNX:ENC-F32     0.25 ONNX:ENC-F32   -0.25 ONNX:ENC-F32
   -0.5 ONNX:ENC-F32    0.25 ONNX:ENC-F32    0.0 ONNX:ENC-F32     0.5 ONNX:ENC-F32
   0.25 ONNX:ENC-F32    0.25 ONNX:ENC-F32   -0.5 ONNX:ENC-F32     0.0 ONNX:ENC-F32
   0.0 ONNX:ENC-F32    -0.5 ONNX:ENC-F32     0.25 ONNX:ENC-F32    0.25 ONNX:ENC-F32
   0.75 ONNX:ENC-F32   -0.25 ONNX:ENC-F32   -0.25 ONNX:ENC-F32    0.5 ONNX:ENC-F32
   -0.25 ONNX:ENC-F32   0.5 ONNX:ENC-F32     0.5 ONNX:ENC-F32    -0.5 ONNX:ENC-F32
   0.5 ONNX:ENC-F32     0.5 ONNX:ENC-F32     0.0 ONNX:ENC-F32     0.25 ONNX:ENC-F32
   -0.75 ONNX:ENC-F32   0.0 ONNX:ENC-F32     0.25 ONNX:ENC-F32   -0.25 ONNX:ENC-F32 ;

: CRF-W2-DATA ( -- )                           \ w2 8x2 row-major raw_data
   0.5 ONNX:ENC-F32    -0.25 ONNX:ENC-F32
   0.25 ONNX:ENC-F32    0.5 ONNX:ENC-F32
   -0.5 ONNX:ENC-F32    0.25 ONNX:ENC-F32
   0.0 ONNX:ENC-F32     0.5 ONNX:ENC-F32
   0.25 ONNX:ENC-F32   -0.5 ONNX:ENC-F32
   0.5 ONNX:ENC-F32     0.0 ONNX:ENC-F32
   -0.25 ONNX:ENC-F32   0.25 ONNX:ENC-F32
   0.25 ONNX:ENC-F32    0.5 ONNX:ENC-F32 ;

public

\ build the composed ModelProto into the encode builder; CRF-MODEL$ returns the finished bytes
: CRF-MODEL ( -- )
   ONNX:ENC-RESET
   8 1 ONNX:ENC-INT                            \ ir_version 8
   7 ONNX:ENC-SUB                              \ graph
      s" x" s" w1t" s" h" CRF-GEMM-TB+         \ layer 1: composed transB Gemm
      s" h" s" a" CRF-RELU+                    \ relu
      s" a" s" w2" s" y" CRF-GEMM+             \ layer 2: plain Gemm
      s" CMLP" 2 ONNX:ENC-STR
      s" w1t" CRF-HID CRF-IN  CRF-INIT2(  CRF-W1-DATA  )CRF-INIT
      s" w2"  CRF-HID CRF-OUT CRF-INIT2(  CRF-W2-DATA  )CRF-INIT
      s" x" 11 CRF-BATCH CRF-IN  CRF-VI+
      s" y" 12 CRF-BATCH CRF-OUT CRF-VI+
   ONNX:;ENC-SUB
   CRF-OPSET+ ;

: CRF-MODEL$ ( -- ptr u8 n )  CRF-MODEL  ONNX:ENC$ ;   \ the composed ModelProto bytes

private
: CRF-WA-DATA ( -- )                           \ w 4x2 row-major raw_data (alpha probe)
   0.5 ONNX:ENC-F32    -0.25 ONNX:ENC-F32
   0.25 ONNX:ENC-F32    0.5 ONNX:ENC-F32
   -0.5 ONNX:ENC-F32    0.25 ONNX:ENC-F32
   0.0 ONNX:ENC-F32     0.5 ONNX:ENC-F32 ;
: CRF-GEMM-ALPHA+ ( ptr u8 n ptr u8 n ptr u8 n -- )   \ Gemm x w -> out, alpha=2 (composed scale)
   {: xa:ptr xu:n wa:ptr wu:n oa:ptr ou:n :}
   1 ONNX:ENC-SUB
      xa xu 1 ONNX:ENC-STR  wa wu 1 ONNX:ENC-STR  oa ou 2 ONNX:ENC-STR
      s" Gemm" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" alpha" 1 ONNX:ENC-STR  2.0 2 ONNX:ENC-F32A  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;
public

\ NEGATIVE fixture: a composed alpha<>1 Gemm (MATMUL + inserted OP-SCALE). OP-SCALE is NOT a v1
\ matmul epilogue op (maki/lower-mm.f LMM-EPI-OP? accepts only relu/gelu/silu), so its matmul
\ region is NOT device-lowerable and the device matmul route rejects it FAIL-CLOSED (E-LMM-OP).
\ The sibling test proves that fail-closed rejection; this is the documented alpha/bias residual.
: CRF-ALPHA-MODEL ( -- )
   ONNX:ENC-RESET
   8 1 ONNX:ENC-INT
   7 ONNX:ENC-SUB
      s" x" s" wa" s" y" CRF-GEMM-ALPHA+
      s" QAB" 2 ONNX:ENC-STR
      s" wa" CRF-IN CRF-OUT CRF-INIT2(  CRF-WA-DATA  )CRF-INIT
      s" x" 11 CRF-BATCH CRF-IN  CRF-VI+
      s" y" 12 CRF-BATCH CRF-OUT CRF-VI+
   ONNX:;ENC-SUB
   CRF-OPSET+ ;

: CRF-ALPHA-MODEL$ ( -- ptr u8 n )  CRF-ALPHA-MODEL  ONNX:ENC$ ;   \ composed alpha ModelProto bytes

\ ---- committed runtime input (exact 2^-2-grid f32, so host f64 == device f32) ---
create CRF-X CRF-XN cells allot

0.5  CRF-X 0 T-SET   0.25 CRF-X 1 T-SET   -0.5 CRF-X 2 T-SET   1.0  CRF-X 3 T-SET
0.75 CRF-X 4 T-SET  -0.25 CRF-X 5 T-SET    0.5 CRF-X 6 T-SET  -1.0  CRF-X 7 T-SET

\ ---- host-oracle reference output (EX-RUN on CRF-X; exact rationals) ------------
\ Provenance: the maki host executor (validated == onnxruntime within 1e-5 on the ort-ref
\ fixture). The sibling test re-derives these from EX-RUN and pins them (host regression), then
\ goldens the DEVICE output against them under the composed device-vs-host tolerance.
create CRF-Y CRF-YN cells allot

-0.0625     CRF-Y 0 T-SET   -0.046875   CRF-Y 1 T-SET
0.59375     CRF-Y 2 T-SET   -0.1875     CRF-Y 3 T-SET

;package
