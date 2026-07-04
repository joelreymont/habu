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

TEST:GROUP-SEQUENTIAL maki
TEST:SUITE maki/array-test.f
TEST:END-SUITE
TEST:SUITE maki/tensor-test.f
TEST:END-SUITE
TEST:SUITE maki/tensor-value-test.f
TEST:END-SUITE
TEST:SUITE maki/optim-test.f
TEST:END-SUITE
TEST:SUITE maki/optim-tensor-test.f
TEST:END-SUITE
TEST:SUITE maki/loss-test.f
TEST:END-SUITE
TEST:SUITE maki/autograd-test.f
TEST:END-SUITE
TEST:SUITE maki/fmath-test.f
TEST:END-SUITE
TEST:SUITE maki/softmax-test.f
TEST:END-SUITE
TEST:SUITE maki/celoss-test.f
TEST:END-SUITE
TEST:SUITE maki/matmul-test.f
TEST:END-SUITE
TEST:SUITE maki/linear-test.f
TEST:END-SUITE
TEST:SUITE maki/autograd-tensor-test.f
TEST:END-SUITE
TEST:SUITE maki/loss-tensor-test.f
TEST:END-SUITE
TEST:SUITE maki/layernorm-test.f
TEST:END-SUITE
TEST:SUITE maki/gelu-test.f
TEST:END-SUITE
TEST:SUITE maki/silu-test.f
TEST:END-SUITE
TEST:SUITE maki/rmsnorm-test.f
TEST:END-SUITE
TEST:SUITE maki/rope-test.f
TEST:END-SUITE
TEST:SUITE maki/move-test.f
TEST:END-SUITE
TEST:SUITE maki/move-facts-test.f
TEST:END-SUITE
TEST:SUITE maki/op-registry-test.f
TEST:END-SUITE
TEST:SUITE maki/model-ir-test.f
TEST:END-SUITE
TEST:SUITE maki/fusion-plan-test.f
TEST:END-SUITE
TEST:SUITE maki/traffic-test.f
TEST:END-SUITE
TEST:SUITE maki/mem-plan-test.f
TEST:END-SUITE
TEST:SUITE maki/schedule-test.f
TEST:END-SUITE
TEST:SUITE maki/sched-key-test.f
TEST:END-SUITE
TEST:SUITE maki/store-test.f
TEST:END-SUITE
TEST:SUITE maki/store-replay-test.f
TEST:END-SUITE
TEST:SUITE maki/embedding-test.f
TEST:END-SUITE
TEST:SUITE maki/attention-test.f
TEST:END-SUITE
TEST:SUITE maki/mlp-test.f
TEST:END-SUITE
TEST:SUITE maki/train-test.f
TEST:END-SUITE
TEST:SUITE maki/onnx-test.f
TEST:END-SUITE
TEST:SUITE maki/eval-test.f
TEST:END-SUITE
TEST:SUITE maki/fusion-test.f
TEST:END-SUITE
TEST:SUITE maki/report-test.f
TEST:END-SUITE
TEST:SUITE maki/cad-test.f
TEST:END-SUITE
TEST:SUITE maki/cuda-types-test.f
TEST:END-SUITE
TEST:SUITE maki/cuda-driver-test.f
TEST:END-SUITE
TEST:SUITE maki/device-artifacts-test.f
TEST:END-SUITE
TEST:SUITE maki/eval-fixture.f
TEST:END-SUITE
TEST:SUITE maki/eval-repair.f
TEST:END-SUITE
TEST:SUITE maki/device-smoke.f
TEST:END-SUITE
TEST:END-GROUP

end-package

TEST:RUN
