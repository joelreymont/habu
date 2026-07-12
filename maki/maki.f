\ maki/maki.f - Maki's one-file entry point: framework aggregator + curated
\ MAKI: surface.
\
\ Load this single file to bring up the host Maki framework (model authoring,
\ training, losses, optimizers, ONNX import, the checker-as-judge eval core) and
\ to get a curated top-level MAKI: interface by re-export (docs/forth.md
\ "Packages", EXPORT). Two access modes after one load:
\   - call the curated workflow words as MAKI:WORD (this file's EXPORT block); or
\   - drill into a subsystem package directly (LOSS:, OPTIM:, ONNX:, EVAL:,
\     PLAN:, TENSOR:, REPORT:, ...) for that package's full surface.
\
\ CURATION CRITERION. A subsystem public word is re-exported into MAKI: iff it is
\ (1) a word an end user calls directly in the model-authoring or train/eval
\ workflow (a loss, an optimizer step, a model import, a checker-as-judge
\ metric), (2) its bare tail reads unambiguously at the top level (no package
\ stem needed for meaning), and (3) its tail collides with no existing
\ package-MAKI word nor another curated export. EXPORT is tail-preserving, so
\ collisions are resolved by curation (skip / pick the canonical one), never by
\ renaming.
\
\ Deliberately NOT re-exported - drill into the package instead:
\   - the model-builder DSL that reads best stemmed: PLAN:LINEAR, PLAN:MATMUL,
\     PLAN:GELU, ... (PLAN:LINEAR / PLAN:MATMUL also tail-collide with the CPU
\     reference kernels MAKI:LINEAR / MAKI:MATMUL);
\   - the tensor-value store / descriptor-plan internals TENSOR:TV-* / PLAN-*
\     (the planning core, not a user authoring surface);
\   - report builder drivers + field accessors with generic tails
\     (REPORT:NEW / RENDER / *! / *@ / *$);
\   - ONNX proto/encode internals (PB-* / WT-* / ENC-*) and ONNX:LOWER
\     (tail-collides with MAKI:LOWER);
\   - the device / GPU execution + device-golden grading layer (GPU:*,
\     EVAL:GRADE-* / DEVICE-* / REPAIR-*) which needs the device/PTX prelude and
\     reads best stemmed.
\ Excluded loss helpers (not user-facing losses): RES2, NLL-PREC, VAR-OK,
\ HUBER-DELTA (internal predicates / parameters).
\
\ One concern: this file is ONLY the aggregate require list plus the curated
\ EXPORT block; it defines no new behavior of its own.
\
\ Run / check: bin/hb --load maki/maki.f

\ --- framework aggregator ------------------------------------------------------
\ Host-clean subsystem roots; require transitively pulls the authoring / train /
\ lowering / report / plan / tensor / onnx / eval core (the same closure the maki
\ gate proves loadable). No device roots (gpu.f, eval-device*.f) - those keep
\ their own device/PTX prelude.
require maki/cad.f
require maki/golden-artifact.f
require maki/from-scratch-train.f
require maki/train.f
require maki/mlp.f
require maki/attention.f
require maki/embedding.f
require maki/rope.f
require maki/onnx/import.f
require maki/loss-tensor.f
require maki/celoss.f
require maki/optim-tensor.f
require maki/eval.f

\ --- curated MAKI: surface via tail-preserving re-export -----------------------
package MAKI

public

\ loss functions (scalar) + their VJPs
EXPORT LOSS:MSE
EXPORT LOSS:MSE-GRAD
EXPORT LOSS:L1
EXPORT LOSS:NLL
EXPORT LOSS:NLL-MU-GRAD
EXPORT LOSS:NLL-LOGVAR-GRAD
EXPORT LOSS:MAHALANOBIS
EXPORT LOSS:MAHALANOBIS-GRAD
EXPORT LOSS:HUBER
EXPORT LOSS:HUBER-GRAD
EXPORT LOSS:CE
EXPORT LOSS:SOFTMAX-CE-BWD

\ loss functions (tensor-graph) + their cotangents
EXPORT LOSS:TT-MSE
EXPORT LOSS:TT-MSE-DY
EXPORT LOSS:TT-NLL
EXPORT LOSS:TT-NLL-DMU
EXPORT LOSS:TT-NLL-DLV
EXPORT LOSS:TT-MAHALANOBIS
EXPORT LOSS:TT-MAHALANOBIS-DMU
EXPORT LOSS:TT-HUBER
EXPORT LOSS:TT-HUBER-DY

\ optimizer steps
EXPORT OPTIM:SGD
EXPORT OPTIM:SGD-MOM
EXPORT OPTIM:WEIGHT-DECAY
EXPORT OPTIM:ADAM
EXPORT OPTIM:ADAM-M
EXPORT OPTIM:ADAM-V
EXPORT OPTIM:ADAM-W
EXPORT OPTIM:TT-ADAM!

\ model import
EXPORT ONNX:IMPORT
EXPORT ONNX:IMPORT-FILE

\ checker-as-judge eval core
EXPORT EVAL:CHECK-PASSES?
EXPORT EVAL:PASS@1?

;package
