# Maki eval — checker-as-judge + the vs-Triton matrix

`maki/eval.f`: the thesis's judge — the CHECKER scores each candidate kernel (certify =
pass), with pass@1 / pass@k tallying. The model-generation + repair arm is external;
this is the correctness gate it is scored against.

## The eval matrix (vs real Triton, `docs/eval-triton.md`)
Measured **only against real Triton on the Orin** — no unchecked/eager baseline.
- **Error-catch timing:** both catch name/type errors before running (Habu at *author*
  time, Triton at *compile*); the **stack-discipline class** (missing store, wrong
  arity, extra op) is caught at author time by Habu's checker with zero GPU, but only
  at *runtime* by Triton — and this **compounds on multi-op fused kernels** (a longer
  fused chain has more structural error surface; `docs/eval-triton.md`).
- **Model-driven pass@k + repair:** independent subagent generators per task/target,
  graded through each full device loop; SAXPY 5/5 both, softmax + fused-relu likewise,
  with diagnostic-guided repair on the Habu side.
- **Bandwidth/throughput:** memory-bound kernels at the DRAM roof (parity); compute
  kernels placed on the roofline (`docs/kernel-principles.md`).

## Graders + device gate
`maki/eval-device.f` grades `certify AND run-correct` (emit → ptxas → device golden);
`maki/eval-compare.f` is the internal checker-ablation. The committed device-correctness
regressions for the GEMM/attention kernels are `tools/ptx/{matmul,attention}-device-test.f`.

## Design intent + roadmap
Retire the remaining `/tmp` graders into committed checked-Habu tools
(`habu-commit-checked-habu`), run the live-model sampled pass@k
(`habu-eval-matrix-live`) and real generation-token eval (`habu-eval-real-gen`), so the
whole matrix is reproducible from the committed tree. **No "better target" claim beyond
what the committed, measured matrix supports.**
