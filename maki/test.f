\ maki/test.f - Maki's own checked test-suite entry point.
\ Run: bin/hb --load maki/test.f

\ The per-file timed RUN-FILE harness lives in maki/test-harness.f, shared with
\ the parallel gate slices (maki/test-<slice>.f). This file is the full standalone
\ run-all inventory and the coverage-lint master: every suite below must land in
\ exactly one slice (tools/suite-coverage-lint-core.f enforces the partition).
require maki/test-harness.f

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
TEST:SUITE maki/causal-test.f
TEST:;SUITE
TEST:SUITE maki/celoss-test.f
TEST:;SUITE
TEST:SUITE maki/matmul-test.f
TEST:;SUITE
TEST:SUITE maki/extent-test.f
TEST:;SUITE
TEST:SUITE maki/extent-tensor-test.f
TEST:;SUITE
TEST:SUITE maki/spec-test.f
TEST:;SUITE
TEST:SUITE maki/spec-attention-test.f
TEST:;SUITE
TEST:SUITE maki/linear-test.f
TEST:;SUITE
TEST:SUITE maki/autograd-tensor-test.f
TEST:;SUITE
TEST:SUITE maki/loss-tensor-test.f
TEST:;SUITE
TEST:SUITE maki/layernorm-test.f
TEST:;SUITE
TEST:SUITE maki/layernorm-affine-test.f
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
TEST:SUITE maki/lower/ew-test.f
TEST:;SUITE
TEST:SUITE maki/lower/red-test.f
TEST:;SUITE
TEST:SUITE maki/lower/mm-test.f
TEST:;SUITE
TEST:SUITE maki/lower/mv-test.f
TEST:;SUITE
TEST:SUITE maki/lower/model-test.f
TEST:;SUITE
TEST:SUITE maki/precision-test.f
TEST:;SUITE
TEST:SUITE maki/prec-attr-test.f
TEST:;SUITE
TEST:SUITE maki/prec-grammar-test.f
TEST:;SUITE
TEST:SUITE maki/backward-test.f
TEST:;SUITE
TEST:SUITE maki/mlp-bwd-test.f
TEST:;SUITE
TEST:SUITE maki/executor-test.f
TEST:;SUITE
TEST:SUITE maki/equation-op-test.f
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
TEST:SUITE maki/cp-async-legal-test.f
TEST:;SUITE
TEST:SUITE maki/store-test.f
TEST:;SUITE
TEST:SUITE maki/store-replay-test.f
TEST:;SUITE
TEST:SUITE maki/store-rehydrate-test.f
TEST:;SUITE
TEST:SUITE maki/store-frame-test.f
TEST:;SUITE
TEST:SUITE maki/embedding-test.f
TEST:;SUITE
TEST:SUITE maki/attention-test.f
TEST:;SUITE
TEST:SUITE maki/segment-test.f
TEST:;SUITE
TEST:SUITE maki/mha-test.f
TEST:;SUITE
TEST:SUITE maki/batch-loader-test.f
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
TEST:SUITE maki/onnx/deploy-test.f
TEST:;SUITE
TEST:SUITE maki/onnx/ort-ref-test.f
TEST:;SUITE
TEST:SUITE maki/eval/eval-test.f
TEST:;SUITE
TEST:SUITE maki/fusion-test.f
TEST:;SUITE
TEST:SUITE maki/ablate-fusion-test.f
TEST:;SUITE
TEST:SUITE maki/report-test.f
TEST:;SUITE
TEST:SUITE maki/cad-kinds-test.f
TEST:;SUITE
TEST:SUITE maki/target/target-test.f
TEST:;SUITE
TEST:SUITE maki/artifact-test.f
TEST:;SUITE
TEST:SUITE maki/db/artifact-test.f
TEST:;SUITE
TEST:SUITE maki/db/transaction-test.f
TEST:;SUITE
TEST:SUITE maki/db/commit-store-test.f
TEST:;SUITE
TEST:SUITE maki/db/commit-store-crash-test.f
TEST:;SUITE
TEST:SUITE maki/db/diagnostic-test.f
TEST:;SUITE
TEST:SUITE maki/db/obligation-test.f
TEST:;SUITE
TEST:SUITE maki/db/evidence-test.f
TEST:;SUITE
TEST:SUITE maki/db/evidence-applicability-test.f
TEST:;SUITE
TEST:SUITE maki/db/promotion-policy-test.f
TEST:;SUITE
TEST:SUITE maki/db/promotion-authority-test.f
TEST:;SUITE
TEST:SUITE maki/db/promotion-test.f
TEST:;SUITE
TEST:SUITE maki/db/action-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-suite-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-suite-id-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-runner-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-runner-tensor-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-runner-spawn-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-runner-inject-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-case-store-test.f
TEST:;SUITE
TEST:SUITE maki/db/diff-case-store-xproc-test.f
TEST:;SUITE
TEST:SUITE maki/db/capbud-test.f
TEST:;SUITE
TEST:SUITE maki/db/agent-loop-test.f
TEST:;SUITE
TEST:SUITE maki/db/audit-log-test.f
TEST:;SUITE
TEST:SUITE maki/db/commit-store-discharge-test.f
TEST:;SUITE
TEST:SUITE maki/experiment/run-test.f
TEST:;SUITE
TEST:SUITE maki/experiment/run-metric-test.f
TEST:;SUITE
TEST:SUITE maki/experiment/run-lineage-test.f
TEST:;SUITE
TEST:SUITE maki/cad-test.f
TEST:;SUITE
TEST:SUITE maki/cad-replay-test.f
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
TEST:SUITE maki/layernorm-affine-op-test.f
TEST:;SUITE
TEST:SUITE maki/bcast-mul-op-test.f
TEST:;SUITE
TEST:SUITE maki/demo-ffn-test.f
TEST:;SUITE
TEST:SUITE maki/from-scratch-model-test.f
TEST:;SUITE
TEST:SUITE maki/from-scratch-train-test.f
TEST:;SUITE
TEST:SUITE maki/from-scratch-test.f
TEST:;SUITE
TEST:SUITE maki/adam-train-test.f
TEST:;SUITE
TEST:SUITE maki/train-state-test.f
TEST:;SUITE
TEST:SUITE maki/adamw-test.f
TEST:;SUITE
TEST:SUITE maki/pos-embed-test.f
TEST:;SUITE
TEST:SUITE maki/gptblock-test.f
TEST:;SUITE
TEST:SUITE maki/adam-torch-ref-test.f
TEST:;SUITE
TEST:SUITE maki/adam-attn-grad-test.f
TEST:;SUITE
TEST:SUITE maki/attn-eq-test.f
TEST:;SUITE
TEST:SUITE maki/checkpoint-test.f
TEST:;SUITE
TEST:SUITE maki/async-dag-test.f
TEST:;SUITE
TEST:SUITE maki/plan-ir-test.f
TEST:;SUITE
TEST:SUITE maki/typestate-test.f
TEST:;SUITE
TEST:SUITE maki/numpolicy-test.f
TEST:;SUITE
TEST:SUITE maki/schema-test.f
TEST:;SUITE
TEST:SUITE maki/producer-test.f
TEST:;SUITE
TEST:SUITE maki/config-test.f
TEST:;SUITE
TEST:SUITE maki/journal-test.f
TEST:;SUITE
TEST:SUITE maki/rev-test.f
TEST:;SUITE
TEST:SUITE maki/db/keywire-xproc-test.f
TEST:;SUITE
TEST:SUITE maki/db/keywire-xproc-env-test.f
TEST:;SUITE
TEST:SUITE maki/db/audit-log-xproc-test.f
TEST:;SUITE
TEST:SUITE maki/evidence/schema-test.f
TEST:;SUITE
TEST:SUITE maki/evidence/policy-test.f
TEST:;SUITE
TEST:SUITE maki/evidence/policy-e2e-test.f
TEST:;SUITE
TEST:SUITE maki/evidence/promote-test.f
TEST:;SUITE
TEST:SUITE maki/competitive-report-test.f
TEST:;SUITE
TEST:SUITE maki/competitive-store-test.f
TEST:;SUITE
TEST:SUITE tools/eval-triton-test.f
TEST:;SUITE
TEST:SUITE maki/competitive-evidence-test.f
TEST:;SUITE
TEST:SUITE maki/competitive-evidence-store-test.f
TEST:;SUITE
TEST:SUITE maki/cuda-types-test.f
TEST:;SUITE
TEST:SUITE maki/cuda-driver-test.f
TEST:;SUITE
TEST:SUITE maki/device-artifacts-test.f
TEST:;SUITE
TEST:SUITE maki/eval/fixture.f
TEST:;SUITE
TEST:SUITE maki/eval/repair.f
TEST:;SUITE
TEST:SUITE maki/eval/repair-ab-test.f
TEST:;SUITE
TEST:SUITE maki/eval/repair-mech-test.f
TEST:;SUITE
TEST:SUITE maki/eval/passk-test.f
TEST:;SUITE
TEST:SUITE maki/eval/transcript-test.f
TEST:;SUITE
TEST:SUITE maki/eval/matrix-test.f
TEST:;SUITE
TEST:SUITE maki/eval/matrix-main.f
TEST:;SUITE
TEST:SUITE maki/eval/live-test.f
TEST:;SUITE
TEST:SUITE maki/eval/tokest-test.f
TEST:;SUITE
TEST:SUITE maki/eval/emit-test.f
TEST:;SUITE
TEST:SUITE maki/eval/live-author-test.f
TEST:;SUITE
TEST:SUITE maki/eval/emit-device-test.f
TEST:;SUITE
TEST:SUITE maki/eval/device-fault-test.f
TEST:;SUITE
TEST:SUITE maki/eval/train.f
TEST:;SUITE
TEST:SUITE maki/maki-test.f
TEST:;SUITE
TEST:SUITE maki/gpu-emit-test.f
TEST:;SUITE
TEST:SUITE maki/device-smoke.f
TEST:;SUITE
TEST:SUITE maki/tokenizer-test.f
TEST:;SUITE
TEST:SUITE maki/data-loader-test.f
TEST:;SUITE
TEST:SUITE maki/xent-loss-test.f
TEST:;SUITE
TEST:SUITE maki/xent-train-test.f
TEST:;SUITE
TEST:;GROUP

TEST:RUN
