# Maki autograd — design & working

Reverse-mode automatic differentiation (backpropagation) for maki, in **two layers**.
The headline property is not coverage (yet) but **correctness by construction**: the
backward is automatically derived for kernels, the derived backward is itself a
**type-checked** kernel, and every rule is **numerically gradchecked**.

Terminology: the per-op backward rule is a **VJP** (vector-Jacobian product), a.k.a. the
*adjoint* / *pullback*. The whole reverse sweep is **reverse-mode AD**.

---

## Layer 1 — `maki/autograd.f`: element-level VJP rules (the op table)

Each tensor op is a forward word paired with its backward (VJP) word, at the element
level so it runs in Habu floats and can be numerically verified:

| op | forward | VJP (backward) |
|----|---------|----------------|
| `ADD` | `ADD-F (x y -- z)` = `x+y` | `ADD-BWD (dz -- dx dy)` = `dz, dz` (linear: copy cotangent to both) |
| `MUL` | `MUL-F (x y -- z)` = `x·y` | `MUL-BWD (dz x y -- dx dy)` = `dz·y, dz·x` (needs saved `x,y`) |
| `RELU` | `RELU-F (x -- z)` = `max(x,0)` | `RELU-BWD (dz x -- dx)` = `x<0 ? 0 : dz` (gates on saved sign) |

Each VJP is the transpose of the op's linearization. **Linear** ops (ADD) need no saved
state; **nonlinear** ops (MUL, RELU) consume saved primals/outputs.

### Gradcheck (the verification)

`maki/autograd-test.f` asserts every analytic VJP equals the **central finite difference**
`(f(x+h) − f(x−h)) / 2h`, `h = 0.001` — e.g. `d(x·y)/dx` at `x=3,y=4` computed both as
`MUL-BWD` (→ 4) and as `(MUL-F(3.001,4) − MUL-F(2.999,4))/0.002` (→ 4). This makes "the
gradient is correct" an *actually verified* claim, not a hand-wave.

---

## Layer 2 — `lib/ptx/ad.f`: automatic source-to-source reverse pass (kernels)

For a checked **kernel** (a concatenative word sequence), the backward is derived
**automatically** by a *syntactic reversal* — no runtime tape:

```
AD-REVERSE ( forward-body -- backward-body ):
  AD-TOKENIZE   split the forward body into whitespace tokens
  AD-EMIT-REV   emit the tokens in REVERSE order, each replaced by its adjoint
```

A forward pipeline `w1 w2 … wn` has gradient `VJP[wn] … VJP[w1]`. The adjoint of each
token comes from two tables:

- **`VJP-ADJOINT`** — the *linear, data-free* primitives, which are **mutual adjoints**:
  `+.`↔`DUP`, `BLOCK-SUM`↔`BROADCAST`, `LOAD`↔`STORE`, `ROW-LOAD`↔`ROW-STORE`, `NEG`↔`NEG`.
  (Reverse of a reduce is a broadcast; reverse of a load is a store; etc.)
- **`VJP-EXPAND`** — the *nonlinear* ops, whose adjoint is a multi-word **expansion** that
  references saved primals/outputs (`SAVED-X` / `SAVED-Y` / `SAVED-MX` / …):
  - `EXP.` → `SAVED-Y *.` (dz·y, y = saved output)
  - `BLOCK-MAX` → `SAVED-X SAVED-MX BLOCK-MAX-SELECT` (scatter cotangent to the arg-max lane)
  - `*.` → `DUP SAVED-Y *. SWAP SAVED-X *.` (dx=dz·y, dy=dz·x — 2-output, cotangents
    threaded by stack juggling)
  - `B-` → `DUP BLOCK-SUM NEG` (dt=dz, ds=−Σdz)

The derived backward is **an ordinary checked kernel** — it type-checks through the same
stack-effect checker as any forward kernel.

### Save-vs-recompute (`VJP-SAVES`)

The tape's replacement: each op declares how many forward values its backward needs
(`EXP.`→1, `*.`→2, `B/`→2 (s, z), linear ops →0). Finite and known at compile time. The
cost model (`habu-ad-save-vs`) chooses save vs recompute per value.

### v0 scope (named, dotted boundary)

Straight-line pipelines; linear mutual-adjoints + the listed nonlinear expansions; 1:1
cotangent threading (general fan-out is scatter-add, `habu-ad-scatter-add`); no control
flow. The remaining primitive adjoints (`B/`, full multi-output threading) and the
saved-value buffers (`SAVED-*` → real buffers, `habu-ad-thread-saved`) are dotted.

---

## How the two layers relate

`maki/autograd.f` is the **element/tensor-op** view (what a model author composes);
`lib/ptx/ad.f` is the **kernel** view (how a fused kernel's backward is emitted). The
maki element rules lower onto the PTX primitive VJP table; the PTX reverse pass emits the
device backward kernel. The DAG-validated variant lives in `lib/ptx/ad-dag.f`.

---

## vs PyTorch (honest)

| | PyTorch | maki |
|---|---------|------|
| mechanism | **dynamic tape** (define-by-run): record the graph at runtime, walk it back | **source-to-source** (define-then-transform): the program structure *is* the tape, reversed at compile time — **tape-free** (closer to JAX/Zygote/Tapenade) |
| per-op backward | hand-written, **trusted** (silently wrong until you gradcheck) | hand-written rule **but** the derived backward kernel is **type-checked** *and* **gradchecked** |
| coverage | hundreds of ops, arbitrary control flow, batched GPU tensors, higher-order | **early/partial**: ~3 scalar ops + a fixed straight-line primitive set |

**Not on par on coverage.** The *win* is correctness-by-construction: a verified-gradient
kernel target rather than a trusted one. Reaching parity-or-better is the epic
`habu-epic-maki-autograd` with sub-dots: tensor/batched VJP layer, transformer-block op
coverage (matmul/attention/layernorm/GELU/residual/embedding), higher-order grad
(differentiate the backward — structurally clean for a source-to-source pass), a committed
**verified-gradient matrix** (every VJP type-checks + gradchecks — the concrete
better-than-PyTorch proof), and end-to-end model-grad parity vs a CPU/PyTorch reference.
