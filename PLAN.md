# Maki + Habu-PTX build plan

**Scope.** Maki is the ML *framework* — tensor/array types, the autograd
*orchestration*, optimizers, ONNX import, the training/eval loop, and the
LLM-target experiment — layered on Habu and its PTX kernel backend. The kernel DSL
("checked Triton"), local type inference, and the reverse-mode autograd
*transform* are Habu language/compiler features; Maki is the framework above them.

This file is the handoff. It is meant to be reviewed (`/review-plan`), split
(`/small-dots`), and implemented by another agent **in this workspace**
(`~/Work/habu-maki`, branch `maki`). It is not yet split into dots.

## Architecture (decided — see this session's discussion + `docs/`)

Three layers, not two:

1. **Habu core** — checked Forth, JIT/AOT, the trust root (`TRUSTED.md`, the
   byte-for-byte fixpoint, the native gate), the arm64 targets. General-purpose.
2. **PTX kernel backend** — `src/arch/ptx/`, `lib/ptx.f`, the `tile<T,B,M>` type
   system, milestones M1–M11, *inference*, the AD *reverse-pass transform*. A
   **codegen target of the Habu compiler** (shares the IR, checker, encoder).
3. **Maki** — the ML framework. A **`maki/` subdirectory**, with a strict one-way
   dependency (**maki → habu**, never the reverse), **fenced out of the trust
   root** (not in `TRUSTED.md`, not in the fixpoint, not a native-gate dependency).

**Guardrails (keep the subdir extractable):**

- One-way imports, enforced: maki loads Habu libraries; Habu core/gate must never
  reference `maki/`.
- Maki carries its own `STATUS.md` / dots namespace; it is application Forth using
  `bin/hb`, so it is naturally outside the self-hosting fixpoint — keep it there.
- Treat the habu↔maki seam as an API even in-repo; that discipline is the future
  repo boundary.
- Extract `maki/` to its own repo when the Habu PTX API stabilizes, maki gets
  external users / a divergent release cadence, or maki churn slows the Habu gate.

## Current state (what already exists)

- `docs/ptx.md`, `docs/ptx-sketch.md` — PTX kernel DSL v0 spec + strategy.
- `docs/inference.md` — local type inference design (infer bodies, annotate the edge).
- `docs/autograd.md` — reverse-mode autograd design + the full VJP table.
- Dots: `habu-ptx-m1 … m11` (the PTX milestone chain), `habu-ptx-local-type`
  (inference, after M4), `habu-ptx-ad-reverse` (autograd, after M6).
- `src/arch/ptx/emit.f` — M3 SAXPY PTX encoder; `tools/ptx/` — emit + ptxas smoke.

## Workstreams

- **A. PTX kernel DSL (Habu).** The existing M1→M11 chain (`ptx-sketch.md`): the
  "checked Triton." Critical path to everything below; M6 (collectives /
  softmax-rows) is the gate for autograd.
- **B. Local type inference (Habu).** `habu-ptx-local-type`, after M4. Drop
  annotations on intermediates; infer from top-of-stack; thread extent/mask/space
  tokens; keep declared effects on recursion/branches; add a `:type` form. Improves
  all kernel authoring; parallel to A.
- **C. Reverse-mode autograd transform (Habu).** `habu-ptx-ad-reverse`, after M6.
  Two new primitives only: **`BROADCAST`** (name the implicit broadcast inside
  `B-`/`B/`) and the **`BLOCK-MAX` arg-max select** (a masked scatter). Then the
  reverse pass, an algebraic-simplify layer, save-vs-recompute, and scatter-add
  (`red.global.add`) for accumulating adjoints. Unblocks `SOFTMAX-ROWS-BWD`.
- **D. Maki framework (new, `maki/`).** Built on A+B+C:
  - tensor/array types above raw tiles (shapes, dtypes, layouts, broadcasting);
  - autograd *orchestration*: the tensor-op VJP rule table + the user-facing
    "define forward → get a checked backward" + optimizer integration;
  - optimizers (SGD/Adam), losses;
  - ONNX import → Habu-PTX kernels (so Spark-trained models deploy here);
  - training loop + gradient checkpointing;
  - the **LLM-target eval harness** (`ptx.md`/M11): Habu-PTX vs Triton —
    pass@k, repair rounds, tokens-to-green, GB/s. No "better target" claim until
    this matrix validates it.

## Sequencing (critical path)

1. Drive the PTX chain to **M6** (A) — prerequisite for autograd.
2. Land **inference** (B, after M4) in parallel — improves all kernels.
3. Name **`BROADCAST`** + the **`BLOCK-MAX` select**; build the **AD reverse pass**
   (C) — unblocks softmax-bwd.
4. **Scaffold `maki/`** (D): subdir, one-way dep, own `STATUS.md`/dots, a README
   pointing here. The framework shell + the habu seam.
5. Maki **tensor types** + **autograd orchestration** on the C transform.
6. **ONNX import** + **training loop** + checkpointing.
7. The **LLM-target eval matrix** (M11 + the maki harness) — the validation that
   earns the thesis.

## Handoff steps (for the picking-up agent)

Work in `~/Work/habu-maki` on branch `maki`. Then:

1. `/review-plan` this `PLAN.md`; fold the review back in.
2. `/small-dots` split workstreams C-prereqs (BROADCAST, BLOCK-MAX select) and D
   (maki) into dots, slotted under the existing chain: BROADCAST / select
   blocked-by M6; maki scaffold blocked-by `habu-ptx-ad-reverse`; maki tensor /
   autograd / ONNX / training / eval chained after.
3. Implement bottom-up per the sequencing. Respect the guardrails (one-way dep,
   trust fence). The **aot-gate WIP lives in the `default` workspace and is out of
   scope** — do not pull it in (this branch was based off the clean docs commit).

## Non-goals / honesty

- Not beating cuBLAS GEMM — `ptx.md` concedes the FLOPS axis. The thesis is
  **checked kernels + verified gradients** as a better target for LLM-authored ML,
  validated by the eval matrix, not by raw throughput.
- No "better LLM target" claim until step 7 produces the data.
