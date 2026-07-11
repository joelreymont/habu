\ maki/onnx/deploy-test.f - imported model -> forward-kernel lowering (inference deploy).
\
\ Dot habu-maki-onnx-import: "the supported-op lowering table to forward kernels;
\ inference deploy needs only forward kernels". This suite PROVES that claim host-side:
\ an ONNX model imported by ONNX:IMPORT lowers through the SAME whole-model deploy
\ machinery a MODEL:-captured plan uses - the fusion plan (FP-BUILD), the per-class
\ region tally + device-lowerability probe (MDL-COUNT-REGIONS / MDL-LOWERABLE?), and a
\ REAL PTX forward kernel emitted per region class the importer can produce:
\   Gemm+Relu -> matmul region  (LMM-EMIT: tiled GEMM, bias add, fused relu epilogue)
\   Add       -> elementwise    (LEW-EMIT: flat kernel, coalesced load, add.rn)
\   Mul+Softmax -> row-reduce   (LRED-EMIT: block-per-row, max/exp/sum/divide)
\   Concat    -> movement copy  (LMV-EMIT: two-source branch copy)
\ Off-device the whole-model golden over the imported IR is an honest not-run
\ (V-NOTRUN); the device run needs the Orin (maki/lower-model-device-test.f pattern).
\ Fixtures are the import-test.f hand-encoded ModelProtos (maki/onnx/encode.f DSL).
\
\ Boundary fact (fail-closed): the CAD command surface (MAKI:LOWER / MAKI:CERTIFY /
\ gates) does NOT adopt an imported model - only MODEL: capture arms it - so on a
\ cleared model state both fail closed E-CAD-NOMODEL even with a fully-imported IR
\ present. When a CAD adoption seam for imported models lands (cad.f-owned), these two
\ negatives must be rewritten into positive LOWER/CERTIFY assertions. Load via maki/test.f.

require lib/test.f
require lib/string.f
require lib/float.f
require src/arch/ptx/emit.f
require maki/cad.f
require maki/onnx/encode.f
require maki/onnx/import.f
require maki/lower-golden.f
require maki/lower-ew.f
require maki/lower-mm.f
require maki/lower-red.f
require maki/lower-move.f

package ONNX-DEPLOY-TEST

\ ---- fixture encoders (the import-test.f ModelProto DSL) -----------------------
: DIM+ ( n -- ) {: d:n :}                      \ one TensorShapeProto.Dimension
   1 ONNX:ENC-SUB  d 1 ONNX:ENC-INT  ONNX:;ENC-SUB ;

: VI+ ( ptr u8 n n n n -- ) {: a:ptr u:n fld:n rows:n cols:n :}   \ 2D f32 ValueInfo
   fld ONNX:ENC-SUB
      a u 1 ONNX:ENC-STR
      2 ONNX:ENC-SUB  1 ONNX:ENC-SUB
         1 1 ONNX:ENC-INT
         2 ONNX:ENC-SUB  rows DIM+  cols DIM+  ONNX:;ENC-SUB
      ONNX:;ENC-SUB  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: MDL ( -- )  ONNX:ENC-RESET  8 1 ONNX:ENC-INT  7 ONNX:ENC-SUB ;  \ ir_version + open graph
: ;MDL ( -- )                                  \ close graph + a skipped opset_import
   ONNX:;ENC-SUB
   8 ONNX:ENC-SUB  13 2 ONNX:ENC-INT  ONNX:;ENC-SUB ;

: NODE1 ( ptr u8 n ptr u8 n ptr u8 n -- )      \ one-input node: op in out
   {: opa:ptr opu:n ia:ptr iu:n oa:ptr ou:n :}
   1 ONNX:ENC-SUB
      ia iu 1 ONNX:ENC-STR  oa ou 2 ONNX:ENC-STR  opa opu 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: NODE2 ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )   \ two-input node: op in0 in1 out
   {: opa:ptr opu:n ia:ptr iu:n ja:ptr ju:n oa:ptr ou:n :}
   1 ONNX:ENC-SUB
      ia iu 1 ONNX:ENC-STR  ja ju 1 ONNX:ENC-STR
      oa ou 2 ONNX:ENC-STR  opa opu 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: GEMM-NODE ( -- )                             \ Gemm x w b -> t
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" w" 1 ONNX:ENC-STR  s" b" 1 ONNX:ENC-STR
      s" t" 2 ONNX:ENC-STR  s" Gemm" 4 ONNX:ENC-STR
   ONNX:;ENC-SUB ;

: INIT-HDR ( ptr u8 n -- ) {: a:ptr u:n :}     \ dims [2,2] f32 + name (payload follows)
   2 1 ONNX:ENC-INT  2 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  a u 8 ONNX:ENC-STR ;

: INIT-W ( -- )                                \ w 2x2 = [[5,6],[7,8]] via raw_data
   5 ONNX:ENC-SUB
      s" w" INIT-HDR
      9 ONNX:ENC-SUB
         5.0 ONNX:ENC-F32  6.0 ONNX:ENC-F32  7.0 ONNX:ENC-F32  8.0 ONNX:ENC-F32
      ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: INIT-B ( -- )                                \ b [2] = [-30,20] rank-1 via raw_data
   5 ONNX:ENC-SUB
      2 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  s" b" 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB  -30.0 ONNX:ENC-F32  20.0 ONNX:ENC-F32  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: INIT-C ( -- )                                \ c 2x2 = [[1,2],[3,4]] via packed float_data
   5 ONNX:ENC-SUB
      s" c" INIT-HDR
      4 ONNX:ENC-SUB
         1.0 ONNX:ENC-F32  2.0 ONNX:ENC-F32  3.0 ONNX:ENC-F32  4.0 ONNX:ENC-F32
      ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: INIT-CC ( -- )                               \ cc 1x2 = [10,20] (concat second operand)
   5 ONNX:ENC-SUB
      1 1 ONNX:ENC-INT  2 1 ONNX:ENC-INT  1 2 ONNX:ENC-INT  s" cc" 8 ONNX:ENC-STR
      9 ONNX:ENC-SUB  10.0 ONNX:ENC-F32  20.0 ONNX:ENC-F32  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: CONCAT-NODE ( -- )                           \ Concat x cc -> y, axis 0
   1 ONNX:ENC-SUB
      s" x" 1 ONNX:ENC-STR  s" cc" 1 ONNX:ENC-STR
      s" y" 2 ONNX:ENC-STR  s" Concat" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" axis" 1 ONNX:ENC-STR  0 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB ;

: MODEL-A ( -- )                               \ Gemm(x,w,b) -> Relu : one matmul region
   MDL
   s" GR2" 2 ONNX:ENC-STR
   GEMM-NODE
   s" Relu" s" t" s" y" NODE1
   INIT-W  INIT-B
   s" x" 11 2 2 VI+  s" y" 12 2 2 VI+
   ;MDL ;

: MODEL-B ( -- )                               \ Add(x,c) : one elementwise region
   MDL
   s" ADD1" 2 ONNX:ENC-STR
   s" Add" s" x" s" c" s" y" NODE2
   INIT-C
   s" x" 11 2 2 VI+  s" c" 11 2 2 VI+  s" y" 12 2 2 VI+
   ;MDL ;

: MODEL-C ( -- )                               \ Mul -> Softmax(axis=-1) : one row-reduce region
   MDL
   s" MS" 2 ONNX:ENC-STR
   s" Mul" s" x" s" x2" s" m" NODE2
   1 ONNX:ENC-SUB
      s" m" 1 ONNX:ENC-STR  s" y" 2 ONNX:ENC-STR  s" Softmax" 4 ONNX:ENC-STR
      5 ONNX:ENC-SUB  s" axis" 1 ONNX:ENC-STR  -1 3 ONNX:ENC-INT  ONNX:;ENC-SUB
   ONNX:;ENC-SUB
   s" x" 11 1 2 VI+  s" x2" 11 1 2 VI+  s" y" 12 1 2 VI+
   ;MDL ;

: MODEL-CC ( -- )                              \ Concat(x 2x2, cc 1x2) axis 0 : one movement region
   MDL
   s" CC" 2 ONNX:ENC-STR
   CONCAT-NODE  INIT-CC
   s" x" 11 2 2 VI+  s" y" 12 3 2 VI+
   ;MDL ;

: IMP! ( -- )  ONNX:ENC$ ONNX:IMPORT  MAKI:FP-BUILD ;   \ import the encoded model + plan regions

\ ---- captured-PTX assertions (ODT- prefix: shares the dictionary with siblings) --
variable ODT-VA  variable ODT-VU
: ODT-SAVE ( ptr u8 n -- )  ODT-VU ! ODT-VA ! ;
: ODT$ ( -- ptr u8 n )  ODT-VA @ ODT-VU @ ;
: ODT-IN ( ptr u8 n -- )  ODT$ 2swap CONTAINS? TTRUE ;            \ needle present

\ exactly-one: the needle occurs once (find it, then prove no second occurrence)
: ODT-ONCE? ( ptr u8 n ptr u8 n -- bool ) {: ha:ptr hu:n na:ptr nu:n :}
   ha hu na nu FIND-SUB {: i1:n :}
   i1 0 < if false exit then
   ha i1 + nu +  hu i1 - nu -  na nu FIND-SUB 0 < ;
: ODT-ONCE ( ptr u8 n -- )  ODT$ 2swap ODT-ONCE? TTRUE ;

: ODT-CAP-MM  ( -- )  PTX-CAPTURE-ON  0 MAKI:LMM-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ ODT-SAVE ;
: ODT-CAP-EW  ( -- )  PTX-CAPTURE-ON  0 MAKI:LEW-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ ODT-SAVE ;
: ODT-CAP-RED ( -- )  PTX-CAPTURE-ON  0 MAKI:LRED-EMIT PTX-CAPTURE-OFF  PTX-CAPTURE$ ODT-SAVE ;
: ODT-CAP-MV  ( -- )  PTX-CAPTURE-ON  0 MAKI:LMV-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ ODT-SAVE ;

\ ---- CAD-surface boundary probes (fail closed until the adoption seam lands) -----
: TRY-CAD-LOWER   ( -- )  MAKI:LOWER drop ;
: TRY-CAD-CERTIFY ( -- )  MAKI:CERTIFY drop ;

\ off-device the whole-model golden over the imported IR is an honest not-run; on-device
\ the real run needs registered cubins (the device test's job), so the assertion skips there.
: ODT-OFFDEV-NOTRUN ( -- )
   CUDA:OPEN? 0= if  MAKI:LOWER-MODEL-GOLDEN MAKI:V-NOTRUN T=  then ;

T-RESET

\ ---- boundary: imported IR alone does not arm the CAD command surface ------------
MAKI:MODEL-CLEAR                               \ deterministic MODEL: state (suite-order-proof)
MODEL-A  ONNX:ENC$ ONNX:IMPORT
' TRY-CAD-LOWER   E-CAD-NOMODEL TTHROWS
' TRY-CAD-CERTIFY E-CAD-NOMODEL TTHROWS

\ ---- imported Gemm+Relu -> ONE matmul region -> the tiled-GEMM forward kernel ----
MODEL-A  IMP!
MAKI:FP-REGION-COUNT 1 T=                      \ Gemm -> Relu fuses (matmul + EW epilogue)
0 MAKI:LLA-REGION-MATMUL? TTRUE
MAKI:MDL-COUNT-REGIONS
MAKI:MDL-N-REGIONS@ 1 T=  MAKI:MDL-N-MM@ 1 T=
MAKI:MDL-LOWERABLE? TTRUE                      \ the whole imported model is device-lowerable
0 MAKI:LMM-ANALYZE
MAKI:LMM-M@ 2 T=  MAKI:LMM-N@ 2 T=  MAKI:LMM-K@ 2 T=
MAKI:LMM-NIN@ 3 T=                             \ x + w + b (OP-LINEAR operands)
ODT-CAP-MM
s" .version 8.3"                ODT-ONCE
s" .visible .entry REGION_0"    ODT-ONCE
s" .param .u64 p_in2"           ODT-IN         \ the bias operand
s" fma.rn.f32"                  ODT-IN         \ K-loop accumulate
s" add.rn.f32"                  ODT-IN         \ acc += bias[col]
s" max.f32"                     ODT-IN         \ fused relu epilogue
s" st.global.f32"               ODT-IN
ODT-OFFDEV-NOTRUN

\ ---- imported Add -> ONE elementwise region -> the flat forward kernel -----------
MODEL-B  IMP!
MAKI:FP-REGION-COUNT 1 T=
MAKI:MDL-COUNT-REGIONS
MAKI:MDL-N-REGIONS@ 1 T=  MAKI:MDL-N-EW@ 1 T=
MAKI:MDL-LOWERABLE? TTRUE
0 MAKI:LEW-ANALYZE
MAKI:LEW-NIN@ 2 T=                             \ x + the materialized c initializer slot
ODT-CAP-EW
s" .visible .entry REGION_0"    ODT-ONCE
s" .param .u64 p_in0"           ODT-IN
s" .param .u64 p_in1"           ODT-IN
s" .param .u32 p_n"             ODT-IN
s" add.rn.f32"                  ODT-IN         \ the Add op chain
s" st.global.f32"               ODT-IN

\ ---- imported Mul+Softmax -> ONE row-reduce region -> the block-per-row kernel ----
MODEL-C  IMP!
MAKI:FP-REGION-COUNT 1 T=                      \ the Mul prologue fuses into the reduction
0 MAKI:LLA-REGION-REDUCE? TTRUE
MAKI:MDL-COUNT-REGIONS
MAKI:MDL-N-REGIONS@ 1 T=  MAKI:MDL-N-RED@ 1 T=
MAKI:MDL-LOWERABLE? TTRUE
ODT-CAP-RED
s" .visible .entry REGION_0"    ODT-ONCE
s" .param .u32 p_k"             ODT-IN
s" mul.rn.f32"                  ODT-IN         \ the fused Mul prologue
s" max.f32"                     ODT-IN         \ block-max fold
s" ex2.approx.f32"              ODT-IN         \ exp
s" div.rn.f32"                  ODT-IN         \ e / sum
ODT-OFFDEV-NOTRUN

\ ---- imported Concat -> ONE materialized movement region -> the copy kernel -------
MODEL-CC  IMP!
MAKI:FP-REGION-COUNT 1 T=
0 MAKI:LLA-REGION-MOVE? TTRUE
MAKI:MDL-COUNT-REGIONS
MAKI:MDL-N-REGIONS@ 1 T=  MAKI:MDL-N-MV@ 1 T=
MAKI:MDL-LOWERABLE? TTRUE
ODT-CAP-MV
s" .visible .entry REGION_0"    ODT-ONCE
s" .param .u64 p_in0"           ODT-IN
s" .param .u64 p_in1"           ODT-IN         \ two source buffers
s" FROMA:"                      ODT-IN         \ e < split branch copy
s" st.global.f32"               ODT-IN
ODT-OFFDEV-NOTRUN

T-REPORT

end-package
