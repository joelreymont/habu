# Maki eval — checker-as-judge + the vs-Triton matrix

`maki/eval.f`: the thesis's judge — the CHECKER scores each candidate kernel (certify =
pass), with pass@1 / pass@k tallying. The model-generation + repair arm is external;
this is the correctness gate it is scored against.

## The eval matrix (vs real Triton, `docs/eval-triton.md`)
The external comparison is measured against **real Triton on the Orin**. The
separate internal no-checker Habu ablation lives in `maki/eval-compare.f`.
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
`maki/eval.f` scores model candidates through `CHECK-CANDIDATE!`, not raw `CHECK!`:
candidate signatures are allowed to shadow existing names during that one check, then
the checker registries are restored so repeated `K`/`A` candidates do not poison the
host dictionary. `maki/eval-device.f` grades `certify AND run-correct` (emit → ptxas →
device golden); `maki/eval-compare.f` is the internal checker-ablation. That ablation
scores every candidate through both `GRADE-CANDIDATE` and a
throwaway `0 set-check` emit/ptxas/device path; on the SAXPY fixture, the checker
catches 5/6 bugs before execution while the no-checker arm catches 0/6 before
execution and all six buggy candidates fail only at the device golden. The committed
device-correctness regressions for the GEMM/attention kernels are
`tools/ptx/{matmul,attention}-device-test.f`.

## Design intent + roadmap
Retire the remaining `/tmp` graders into committed checked-Habu tools
(`habu-commit-checked-habu`), run the live-model sampled pass@k
(`habu-eval-matrix-live`) and real generation-token eval (`habu-eval-real-gen`), so the
whole matrix is reproducible from the committed tree. **No "better target" claim beyond
what the committed, measured matrix supports.**
