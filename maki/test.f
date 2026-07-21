\ maki/test.f - Maki's own checked test-suite entry point.
\ Run: bin/hb --load maki/test.f

\ The per-file timed RUN-FILE harness lives in maki/test-harness.f, shared with
\ the parallel gate slices (maki/test-<slice>.f). This file is the full standalone
\ run-all inventory and the coverage-lint master: every suite below must land in
\ exactly one slice (tools/suite-coverage-lint-core.f enforces the partition).
require maki/test-harness.f

using TEST

RESET

GROUP SEQ maki
SUITE maki/array-test.f
;SUITE
SUITE maki/tensor-test.f
;SUITE
SUITE maki/tensor-value-test.f
;SUITE
SUITE maki/view-test.f
;SUITE
SUITE maki/optim-test.f
;SUITE
SUITE maki/optim-tensor-test.f
;SUITE
SUITE maki/loss-test.f
;SUITE
SUITE maki/autograd-test.f
;SUITE
SUITE maki/fmath-test.f
;SUITE
SUITE maki/softmax-test.f
;SUITE
SUITE maki/sampling-test.f
;SUITE
SUITE maki/causal-test.f
;SUITE
SUITE maki/celoss-test.f
;SUITE
SUITE maki/matmul-test.f
;SUITE
SUITE maki/extent-test.f
;SUITE
SUITE maki/extent-tensor-test.f
;SUITE
SUITE maki/spec-test.f
;SUITE
SUITE maki/spec-attention-test.f
;SUITE
SUITE maki/spec-batched-test.f
;SUITE
SUITE maki/cross-seq-contraction-test.f
;SUITE
SUITE maki/linear-test.f
;SUITE
SUITE maki/autograd-tensor-test.f
;SUITE
SUITE maki/loss-tensor-test.f
;SUITE
SUITE maki/layernorm-test.f
;SUITE
SUITE maki/layernorm-affine-test.f
;SUITE
SUITE maki/gelu-test.f
;SUITE
SUITE maki/silu-test.f
;SUITE
SUITE maki/rmsnorm-test.f
;SUITE
SUITE maki/rope-test.f
;SUITE
SUITE maki/move-test.f
;SUITE
SUITE maki/reduce-bwd-test.f
;SUITE
SUITE maki/scatter-test.f
;SUITE
SUITE maki/move-facts-test.f
;SUITE
SUITE maki/op-registry-test.f
;SUITE
SUITE maki/adjoint-test.f
;SUITE
SUITE maki/model-ir-test.f
;SUITE
SUITE maki/fusion-plan-test.f
;SUITE
SUITE maki/fusion-mout-test.f
;SUITE
SUITE maki/bcast-test.f
;SUITE
SUITE maki/lower/ew-test.f
;SUITE
SUITE maki/lower/red-test.f
;SUITE
SUITE maki/lower/mm-test.f
;SUITE
SUITE maki/lower/mv-test.f
;SUITE
SUITE maki/lower/model-test.f
;SUITE
SUITE maki/lower/launch-leak-test.f
;SUITE
SUITE maki/precision-test.f
;SUITE
SUITE maki/prec-attr-test.f
;SUITE
SUITE maki/prec-grammar-test.f
;SUITE
SUITE maki/backward-test.f
;SUITE
SUITE maki/mlp-bwd-test.f
;SUITE
SUITE maki/executor-test.f
;SUITE
SUITE maki/equation-op-test.f
;SUITE
SUITE maki/saved-test.f
;SUITE
SUITE maki/traffic-test.f
;SUITE
SUITE maki/mem-plan-test.f
;SUITE
SUITE maki/schedule-test.f
;SUITE
SUITE maki/sched-key-test.f
;SUITE
SUITE maki/cp-async-legal-test.f
;SUITE
SUITE maki/store-test.f
;SUITE
SUITE maki/store-replay-test.f
;SUITE
SUITE maki/store-rehydrate-test.f
;SUITE
SUITE maki/store-frame-test.f
;SUITE
SUITE maki/embedding-test.f
;SUITE
SUITE maki/attention-test.f
;SUITE
SUITE maki/segment-test.f
;SUITE
SUITE maki/mha-test.f
;SUITE
SUITE maki/mha-block-test.f
;SUITE
SUITE maki/examples/nanogpt/batch-loader-test.f
;SUITE
SUITE maki/mlp-test.f
;SUITE
SUITE maki/train-test.f
;SUITE
SUITE maki/onnx-test.f
;SUITE
SUITE maki/onnx/proto-test.f
;SUITE
SUITE maki/onnx/import-test.f
;SUITE
SUITE maki/onnx/deploy-test.f
;SUITE
SUITE maki/onnx/ort-ref-test.f
;SUITE
SUITE maki/infer/kv-cache-test.f
;SUITE
SUITE maki/eval/eval-test.f
;SUITE
SUITE maki/fusion-test.f
;SUITE
SUITE maki/ablate-fusion-test.f
;SUITE
SUITE maki/report-test.f
;SUITE
SUITE maki/cad-kinds-test.f
;SUITE
SUITE maki/target/target-test.f
;SUITE
SUITE maki/artifact-test.f
;SUITE
SUITE maki/db/artifact-test.f
;SUITE
SUITE maki/db/transaction-test.f
;SUITE
SUITE maki/db/commit-store-test.f
;SUITE
SUITE maki/db/commit-store-crash-test.f
;SUITE
SUITE maki/db/diagnostic-test.f
;SUITE
SUITE maki/db/obligation-test.f
;SUITE
SUITE maki/db/evidence-test.f
;SUITE
SUITE maki/db/evidence-applicability-test.f
;SUITE
SUITE maki/db/promotion-policy-test.f
;SUITE
SUITE maki/db/promotion-authority-test.f
;SUITE
SUITE maki/db/promotion-test.f
;SUITE
SUITE maki/db/action-test.f
;SUITE
SUITE maki/db/diff-suite-test.f
;SUITE
SUITE maki/db/diff-suite-id-test.f
;SUITE
SUITE maki/db/diff-runner-test.f
;SUITE
SUITE maki/db/diff-runner-tensor-test.f
;SUITE
SUITE maki/db/diff-runner-spawn-test.f
;SUITE
SUITE maki/db/diff-runner-inject-test.f
;SUITE
SUITE maki/db/diff-case-store-test.f
;SUITE
SUITE maki/db/diff-case-store-xproc-test.f
;SUITE
SUITE maki/db/capbud-test.f
;SUITE
SUITE maki/db/agent-loop-test.f
;SUITE
SUITE maki/db/audit-log-test.f
;SUITE
SUITE maki/db/commit-store-discharge-test.f
;SUITE
SUITE maki/experiment/run-test.f
;SUITE
SUITE maki/experiment/run-metric-test.f
;SUITE
SUITE maki/experiment/run-lineage-test.f
;SUITE
SUITE maki/cad-test.f
;SUITE
SUITE maki/cad-replay-test.f
;SUITE
SUITE maki/cad-bind-test.f
;SUITE
SUITE maki/cad-ref-test.f
;SUITE
SUITE maki/plan-compose-test.f
;SUITE
SUITE maki/plan-vocab-test.f
;SUITE
SUITE maki/golden-test.f
;SUITE
SUITE maki/golden-artifact-test.f
;SUITE
SUITE maki/gradcheck-test.f
;SUITE
SUITE maki/layernorm-affine-op-test.f
;SUITE
SUITE maki/bcast-mul-op-test.f
;SUITE
SUITE maki/dropout-op-test.f
;SUITE
SUITE maki/swiglu-op-test.f
;SUITE
SUITE maki/demo-ffn-test.f
;SUITE
SUITE maki/examples/nanogpt/from-scratch-model-test.f
;SUITE
SUITE maki/examples/nanogpt/from-scratch-train-test.f
;SUITE
SUITE maki/examples/nanogpt/from-scratch-test.f
;SUITE
SUITE maki/examples/nanogpt/adam-train-test.f
;SUITE
SUITE maki/train-state-test.f
;SUITE
SUITE maki/adamw-test.f
;SUITE
SUITE maki/pos-embed-test.f
;SUITE
SUITE maki/examples/nanogpt/gptblock-test.f
;SUITE
SUITE maki/examples/nanogpt/adam-torch-ref-test.f
;SUITE
SUITE maki/examples/nanogpt/adam-attn-grad-test.f
;SUITE
SUITE maki/attn-eq-test.f
;SUITE
SUITE maki/examples/nanogpt/gptblock-attn-test.f
;SUITE
SUITE maki/checkpoint-test.f
;SUITE
SUITE maki/async-dag-test.f
;SUITE
SUITE maki/plan-ir-test.f
;SUITE
SUITE maki/typestate-test.f
;SUITE
SUITE maki/numpolicy-test.f
;SUITE
SUITE maki/schema-test.f
;SUITE
SUITE maki/producer-test.f
;SUITE
SUITE maki/config-test.f
;SUITE
SUITE maki/infer/safetensors-test.f
;SUITE
SUITE maki/journal-test.f
;SUITE
SUITE maki/rev-test.f
;SUITE
SUITE maki/db/keywire-xproc-test.f
;SUITE
SUITE maki/db/keywire-xproc-env-test.f
;SUITE
SUITE maki/db/audit-log-xproc-test.f
;SUITE
SUITE maki/evidence/schema-test.f
;SUITE
SUITE maki/evidence/policy-test.f
;SUITE
SUITE maki/evidence/policy-e2e-test.f
;SUITE
SUITE maki/evidence/promote-test.f
;SUITE
SUITE maki/competitive-report-test.f
;SUITE
SUITE maki/competitive-store-test.f
;SUITE
SUITE tools/eval-triton-test.f
;SUITE
SUITE maki/competitive-evidence-test.f
;SUITE
SUITE maki/competitive-evidence-store-test.f
;SUITE
SUITE maki/cuda-types-test.f
;SUITE
SUITE maki/cuda-driver-test.f
;SUITE
SUITE maki/device-artifacts-test.f
;SUITE
SUITE maki/eval/fixture.f
;SUITE
SUITE maki/eval/repair.f
;SUITE
SUITE maki/eval/repair-ab-test.f
;SUITE
SUITE maki/eval/repair-mech-test.f
;SUITE
SUITE maki/eval/passk-test.f
;SUITE
SUITE maki/eval/transcript-test.f
;SUITE
SUITE maki/eval/matrix-test.f
;SUITE
SUITE maki/eval/matrix-main.f
;SUITE
SUITE maki/eval/live-test.f
;SUITE
SUITE maki/eval/tokest-test.f
;SUITE
SUITE maki/eval/emit-test.f
;SUITE
SUITE maki/eval/live-author-test.f
;SUITE
SUITE maki/eval/emit-device-test.f
;SUITE
SUITE maki/eval/emit-device-leak-test.f
;SUITE
SUITE maki/eval/device-fault-test.f
;SUITE
SUITE maki/eval/train.f
;SUITE
SUITE maki/maki-test.f
;SUITE
SUITE maki/gpu-emit-test.f
;SUITE
SUITE maki/gpu-leak-test.f
;SUITE
SUITE maki/device-smoke.f
;SUITE
SUITE maki/examples/nanogpt/tokenizer-test.f
;SUITE
SUITE maki/examples/nanogpt/bpe-test.f
;SUITE
SUITE maki/examples/nanogpt/data-loader-test.f
;SUITE
SUITE maki/xent-loss-test.f
;SUITE
SUITE maki/examples/nanogpt/xent-train-test.f
;SUITE
SUITE maki/examples/nanogpt/generate-test.f
;SUITE
SUITE maki/examples/nanogpt/wtie-train-test.f
;SUITE
;GROUP

RUN

;using
