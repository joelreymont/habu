# The short-stack bet

Recorded 2026-07-25, from a discussion between Joel and the orchestrating
agent, mid-way through the inference-engine build. This is the thesis the
project is testing, stated plainly so future work can be judged against it.

## The bet

The mainstream path from a model to the GPU is a tall stack: PyTorch ->
graph capture -> Inductor/XLA -> Triton -> LLVM -> PTX. Every layer adds
generality — any model, any device, any dtype, autograd through everything
— and then a pile of compiler passes (fusion, layout selection, scheduling)
works to optimize that generality back out. The complexity is paid for
twice: once to build it, once to remove it.

The bet: for the workload we actually care about — large-language-model
inference on one known machine (DGX Spark, GB10) — you can also get there
by starting simple. A short stack, written in checked Habu Forth, that says
exactly what it means all the way down to the PTX it emits itself, skips
the recovery problem entirely. There is nothing to optimize away because
nothing was abstracted in the first place.

## Why the bet is sound

- The generality is unused. Inference of one model family on one chip
  exercises almost none of what the tall stack generalizes over. A decode
  step is roughly a dozen distinct kernels plus memory choreography.
- The workload is bandwidth-bound at decode. The winning move is "put the
  bytes in the right place and don't touch them twice" — a statement you
  can make directly in a short stack, rather than hoping four intermediate
  representations preserve it.
- Precedent: llama.cpp. A deliberately simple, direct C++ implementation
  embarrassed the frameworks on exactly this single-node workload, with
  mapped weights (no copy) — the same design our weight store's mapped arm
  commits to.
- Narrowness collapses the hard problem. Frameworks need compiler heroics
  because they face a combinatorial space of models x devices x dtypes x
  shapes. We face one chip, one dtype family, a handful of shapes.
  Hand-crafting a dozen kernels for one machine, perfectly, is a countable
  amount of work — that is the "beat Triton on 48 SMs" line item, a
  project, not a treadmill.

## Where it gets hard (the honest half)

The last stretch on GEMM and attention is not simplicity — it is
accumulated microarchitectural craft: tensor-core instruction scheduling,
async copy pipelines, swizzled shared-memory layouts. That knowledge lives
inside cuBLAS/CUTLASS and Triton's backends, and a short stack does not get
it for free; it must be earned per kernel, per architecture.

- The forgiving case is decode (latency-critical, bandwidth-bound): a
  straightforward, well-mapped kernel can sit near the roofline.
- The demanding case is batched prefill GEMM: that is where the tall
  stack's inherited craft concentrates, where the bet gets tested hardest,
  and where measurement must be most honest. Expect to fight there.

## The novel part: verification substitutes for abstraction

Simple stacks historically bought directness by giving up safety — raw
C++, hand-tracked invariants, segfaults as the debugging loop. Habu's
version of "simple" keeps directness and adds machine-checked contracts:

- The checker rejected a stack-corrupting generated constructor before it
  could exist (the payload-provider time-of-check/time-of-use hole,
  2026-07-25).
- Linear owner types made "two model loads sharing one mapping" a
  compile-time error, not a runtime heisenbug (the safetensors session
  rework, same day).

The wager underneath the wager: you do not need four layers of IR
protecting you from yourself if the one layer you have refuses to compile
the mistake. If that holds through the kernel work — typed PTX emission
where the checker knows what a tensor-core fragment is — the result sits
in a quadrant nobody occupies: bottom-up direct AND machine-verified, with
LLM workers writing kernels under contracts that a checker, not a
reviewer's stamina, enforces.

## What would falsify it

- Prefill/batched GEMM stuck materially below Triton/cuBLAS on the same
  chip after real kernel-craft investment (not before).
- The checker unable to express the invariants that matter at PTX depth
  (tensor-core fragments, async pipelines) without so many trusted
  boundaries that verification stops paying.
- The per-kernel craft cost growing with each model family until the
  "countable work" claim dies — i.e. the treadmill reappears.

## The near-term test

GPT-2 greedy decode on the Spark, end to end through checked Habu, hitting
a respectable fraction of memory bandwidth with kernels this stack emitted
itself. That single milestone exercises the whole bet: mapped weights, the
typed intake chain, self-emitted PTX, and the claim that a short verified
stack can be fast without a compiler recovering its performance for it.
