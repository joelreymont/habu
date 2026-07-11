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
scores every candidate through both `EVAL:GRADE-CANDIDATE` and a
throwaway `0 set-check` emit/ptxas/device path; on the SAXPY fixture, the checker
catches 5/6 bugs before execution while the no-checker arm catches 0/6 before
execution and all six buggy candidates fail only at the device golden. The committed
device-correctness regressions for the GEMM/attention kernels are
`tools/ptx/{matmul,attention}-device-test.f`.

## The durable pass@k harness: transcripts -> matrix
A generation run is recorded as a plain-text **transcript** file and replayed from the
committed tree — no `/tmp` scripts, no ad hoc subagent logs. Format v1 (line-oriented,
LF; blank lines and `\ ...` comments ignored; one transcript = one target arm):

```
habu-eval-transcript v1
target habu-ptx              once, before any task
task saxpy                   opens/rejoins the task's tally row
sample s1                    one generation sample
candidate <kernel source>    one authoring round: draft, then repairs, in order
result green|fail            recorded verdict for an EXTERNALLY graded sample
                             (e.g. the Triton arm); either candidates OR one result
```

`maki/eval-transcript.f` replays every `candidate` line through the shared repair
metric engine (`maki/eval-repair-loop.f` -> `EVAL:CHECK-PASSES?`, the checker as
judge) and tallies per task: samples, first-attempt greens (the pass@k `c`),
repaired-to-green, repair rounds, tokens-to-green. `maki/eval-passk.f` computes the
unbiased pass@k = 1 − C(n−c,k)/C(n,k) exactly (integer falling factorials, per-mille).
`maki/eval-matrix.f` assembles per-(task,target) rows and renders the recorded matrix
schema; the GB/s and device columns are HOST-HONEST `not-run` placeholders that only
carry values when an on-device run (`tools/ptx/bandwidth.f`; the device goldens behind
`MAKI:GRADE-AUTHOR`) sets them via `EVAL:MATRIX-GBS!` / `EVAL:MATRIX-DEVICE!`. Entry:

```
bin/hb --load maki/eval-matrix-main.f -- run1.txt run2.txt ...
```

With no arguments it replays the committed synthetic fixtures under
`maki/transcripts/` (mirroring the 2026-06-27 recorded round: SAXPY 5/5; softmax 3/5
first-try, repairs green in 1 and 2 rounds -> pass@1/2/3 = 600/900/1000 per-mille),
which is also how the maki suite exercises the exact durable command. The maki model
train/eval leg is `maki/eval-train.f`: it runs the committed Adam MLP + attention
trainers end-to-end and reports steps, milli-loss initial/final, and the committed
convergence verdict.

## Design intent + roadmap
The `/tmp` graders are retired into committed checked-Habu tools, and sampled pass@k
rounds now replay from committed transcript files (`habu-eval-matrix-live`).
Remaining: the live-model generation arm stays external/user-gated (record each run
as a transcript file), the corrected-ROW-STORE softmax re-run (`habu-re-run-habu`),
and the real generation-token eval (`habu-eval-real-gen`). **No "better target" claim
beyond what the committed, measured matrix supports.**
