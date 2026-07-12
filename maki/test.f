\ maki/test.f - Maki's own checked test-suite entry point.
\ Run: bin/hb --load maki/test.f

require lib/test.f
require lib/float.f
require lib/fmt.f

package MAKI-TEST

1000000 constant NS-PER-MS

variable RUN-A
variable RUN-U
variable START-NS

: ELAPSED-MS ( -- n )
   mono-ns START-NS @ - NS-PER-MS / ;

: CURRENT$ ( -- ptr u8 n )
   RUN-A @ RUN-U @ ;

: NUM-TYPE ( n -- )
   SB-RESET SB-INT SB$ type ;

: INCLUDE-CURRENT ( -- )
   CURRENT$ included ;

: RUN-PASS ( -- )
   s" PASS: " type CURRENT$ type
   s"  (" type ELAPSED-MS NUM-TYPE s" ms)" type cr ;

: RUN-FAIL ( n -- ) {: rc:n :}
   s" FAIL: " type CURRENT$ type
   s"  rc=" type rc NUM-TYPE cr
   rc throw ;

: RUN-FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path RUN-A !
   pathu RUN-U !
   s" TEST: " type CURRENT$ type cr
   mono-ns START-NS !
   [: INCLUDE-CURRENT ;] catch {: rc:n :}
   rc 0= if RUN-PASS exit then
   rc RUN-FAIL ;

: INSTALL ( -- )
   [: RUN-FILE ;] TEST:RUNNER! ;

INSTALL
TEST:RESET

TEST:GROUP SEQ maki
TEST:SUITE maki/array-test.f
TEST:;SUITE
TEST:SUITE maki/tensor-test.f
TEST:;SUITE
TEST:SUITE maki/tensor-value-test.f
TEST:;SUITE
TEST:SUITE maki/optim-test.f
TEST:;SUITE
TEST:SUITE maki/optim-tensor-test.f
TEST:;SUITE
TEST:SUITE maki/loss-test.f
TEST:;SUITE
TEST:SUITE maki/autograd-test.f
TEST:;SUITE
TEST:SUITE maki/fmath-test.f
TEST:;SUITE
TEST:SUITE maki/softmax-test.f
TEST:;SUITE
TEST:SUITE maki/celoss-test.f
TEST:;SUITE
TEST:SUITE maki/matmul-test.f
TEST:;SUITE
TEST:SUITE maki/linear-test.f
TEST:;SUITE
TEST:SUITE maki/autograd-tensor-test.f
TEST:;SUITE
TEST:SUITE maki/loss-tensor-test.f
TEST:;SUITE
TEST:SUITE maki/layernorm-test.f
TEST:;SUITE
TEST:SUITE maki/gelu-test.f
TEST:;SUITE
TEST:SUITE maki/silu-test.f
TEST:;SUITE
TEST:SUITE maki/rmsnorm-test.f
TEST:;SUITE
TEST:SUITE maki/rope-test.f
TEST:;SUITE
TEST:SUITE maki/move-test.f
TEST:;SUITE
TEST:SUITE maki/reduce-bwd-test.f
TEST:;SUITE
TEST:SUITE maki/scatter-test.f
TEST:;SUITE
TEST:SUITE maki/move-facts-test.f
TEST:;SUITE
TEST:SUITE maki/op-registry-test.f
TEST:;SUITE
TEST:SUITE maki/adjoint-test.f
TEST:;SUITE
TEST:SUITE maki/model-ir-test.f
TEST:;SUITE
TEST:SUITE maki/fusion-plan-test.f
TEST:;SUITE
TEST:SUITE maki/fusion-mout-test.f
TEST:;SUITE
TEST:SUITE maki/bcast-test.f
TEST:;SUITE
TEST:SUITE maki/lower-ew-test.f
TEST:;SUITE
TEST:SUITE maki/lower-red-test.f
TEST:;SUITE
TEST:SUITE maki/lower-mm-test.f
TEST:;SUITE
TEST:SUITE maki/lower-mv-test.f
TEST:;SUITE
TEST:SUITE maki/lower-model-test.f
TEST:;SUITE
TEST:SUITE maki/precision-test.f
TEST:;SUITE
TEST:SUITE maki/backward-test.f
TEST:;SUITE
TEST:SUITE maki/mlp-bwd-test.f
TEST:;SUITE
TEST:SUITE maki/executor-test.f
TEST:;SUITE
TEST:SUITE maki/saved-test.f
TEST:;SUITE
TEST:SUITE maki/traffic-test.f
TEST:;SUITE
TEST:SUITE maki/mem-plan-test.f
TEST:;SUITE
TEST:SUITE maki/schedule-test.f
TEST:;SUITE
TEST:SUITE maki/sched-key-test.f
TEST:;SUITE
TEST:SUITE maki/store-test.f
TEST:;SUITE
TEST:SUITE maki/store-replay-test.f
TEST:;SUITE
TEST:SUITE maki/embedding-test.f
TEST:;SUITE
TEST:SUITE maki/attention-test.f
TEST:;SUITE
TEST:SUITE maki/mlp-test.f
TEST:;SUITE
TEST:SUITE maki/train-test.f
TEST:;SUITE
TEST:SUITE maki/onnx-test.f
TEST:;SUITE
TEST:SUITE maki/onnx/proto-test.f
TEST:;SUITE
TEST:SUITE maki/onnx/import-test.f
TEST:;SUITE
TEST:SUITE maki/eval-test.f
TEST:;SUITE
TEST:SUITE maki/fusion-test.f
TEST:;SUITE
TEST:SUITE maki/ablate-fusion-test.f
TEST:;SUITE
TEST:SUITE maki/report-test.f
TEST:;SUITE
TEST:SUITE maki/cad-kinds-test.f
TEST:;SUITE
TEST:SUITE maki/cad-test.f
TEST:;SUITE
TEST:SUITE maki/cad-bind-test.f
TEST:;SUITE
TEST:SUITE maki/cad-ref-test.f
TEST:;SUITE
TEST:SUITE maki/plan-compose-test.f
TEST:;SUITE
TEST:SUITE maki/plan-vocab-test.f
TEST:;SUITE
TEST:SUITE maki/golden-test.f
TEST:;SUITE
TEST:SUITE maki/golden-artifact-test.f
TEST:;SUITE
TEST:SUITE maki/gradcheck-test.f
TEST:;SUITE
TEST:SUITE maki/demo-ffn-test.f
TEST:;SUITE
TEST:SUITE maki/from-scratch-model-test.f
TEST:;SUITE
TEST:SUITE maki/from-scratch-train-test.f
TEST:;SUITE
TEST:SUITE maki/from-scratch-test.f
TEST:;SUITE
TEST:SUITE maki/cuda-types-test.f
TEST:;SUITE
TEST:SUITE maki/cuda-driver-test.f
TEST:;SUITE
TEST:SUITE maki/device-artifacts-test.f
TEST:;SUITE
TEST:SUITE maki/eval-fixture.f
TEST:;SUITE
TEST:SUITE maki/eval-repair.f
TEST:;SUITE
TEST:SUITE maki/eval-repair-ab-test.f
TEST:;SUITE
TEST:SUITE maki/eval-repair-mech-test.f
TEST:;SUITE
TEST:SUITE maki/maki-test.f
TEST:;SUITE
TEST:SUITE maki/device-smoke.f
TEST:;SUITE
TEST:;GROUP

;package

TEST:RUN
