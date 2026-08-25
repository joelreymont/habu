# Case: Triton #10927 — TMA descriptor stride misalignment, silent wrong results

The canonical motivating case for the TMA emitter + design-rule work
(`docs/tma-gather.md`). A production-grade Triton kernel produces ~96%-wrong
GEMM results with no error, on current Blackwell hardware, because a documented
hardware precondition is expressed nowhere in the language. In Habu terms: a
missing design-rule check, and the exact bug class the checker exists to shift
to author time.

Upstream: <https://github.com/triton-lang/triton/issues/10927> (B200-confirmed,
found in a vLLM MoE kernel). Fix PR <https://github.com/triton-lang/triton/pull/10931>
adds a build-time error for *statically known* strides only; device-built
descriptors with runtime strides stay unvalidated (an emitted assert was
rejected as runtime overhead). So the mainstream resolution is a partial guard,
not a type-level guarantee.

## The failing kernel (upstream repro, verbatim core)

Triton's tensor-descriptor path exposes TMA plumbing directly as user syntax —
a host-side allocator hook, descriptor construction with raw shape/stride
tuples, and gather/load calls against the descriptor:

```python
triton.set_allocator(_alloc_fn)          # host callback TMA needs for scratch

a_desc = tl.make_tensor_descriptor(
    base=a_ptr, shape=(M, K), strides=(stride_am, stride_ak),
    block_shape=(1, BLOCK_K),
)
b_desc = tl.make_tensor_descriptor(
    base=b_ptr, shape=(N, K), strides=(stride_bn, stride_bk),
    block_shape=(BLOCK_N, BLOCK_K),
)
gather_idx = offs_m.to(tl.int32)

for k in range(0, tl.cdiv(K, BLOCK_K)):
    a = a_desc.gather(gather_idx, k * BLOCK_K)          # [BLOCK_M, BLOCK_K]
    b = b_desc.load([pid_n * BLOCK_N, k * BLOCK_K]).T   # [BLOCK_K, BLOCK_N]
    acc += tl.dot(a, b)
```

With `M,N,K = 128,128,511`, bf16, blocks `32,64,64`: max diff **130.45**,
**96.42%** of elements wrong, deterministic. The identical computation via
pointer arithmetic + masked `tl.load` is exact. Reproduced bit-for-bit on B200
(sm_100, upstream) and on our DGX Spark GB10 (sm_121a) — same TTGIR
(`ttng.async_tma_gather` + `ttng.async_tma_copy_global_to_local`), same
on-device `tensormap.replace.tile.global_stride` descriptor build.

## Root cause: an unchecked alignment precondition

TMA requires **global strides to be multiples of 16 bytes** (and a 16-byte
aligned base; `cuTensorMapEncodeTiled` documents this, and the on-device
`tensormap.replace` path inherits it). `tl.make_tensor_descriptor` states the
requirement in a docstring and checks nothing. bf16 `K=511` → row stride
1022 B → not a multiple of 16 → the descriptor is silently corrupt.

The issue's own framing ("ragged K tile") is wrong, which is why the case is
instructive. Proven on GB10 (scripts in the dgx-spark workspace,
`repro/10927/{repro,hyp_stride,probe_tile,proof_padstride}.py`, writeup
`docs/tma-10927.md` there):

- Ragged-but-aligned K (72, 120, 200, 456, 520): all exact. Only
  `K % 8 != 0` (bf16) fails — 63, 65, 196, 511, 513.
- k-tile 0, fully in-bounds, zero OOB: already ~97% garbage under a
  misaligned stride. The *whole descriptor* is corrupt, not the ragged tail.
- Hold `K=511` fixed, view the 511 columns out of a `128×512` allocation so
  only the row stride changes to 1024 B: flips from 96%-wrong to bit-exact.

## Why this is a Habu design-rule check

The precondition is static in every real instance (strides come from tensor
shapes known at plan time), the failure is silent numeric corruption, and the
mainstream fix cannot make the guarantee total because the plumbing is user
syntax with runtime values. Habu's position per `docs/ptx.md`: movement is
generated routing, not user syntax. The user never builds a descriptor, so the
rule has one enforcement point — the planner/checker at lowering time:

- The type grammar already carries alignment marker atoms (`align-16`,
  `docs/ptx-sketch.md`) and spans carry extents. A TMA lowering is legal only
  for a span whose base and row stride carry 16-byte evidence; the checker
  refuses the lowering otherwise — at author time, naming the rule.
- The planner has an option the language user does not: **pad the allocation**
  (row stride up to the next 16-byte multiple) and keep TMA, or fall back to
  `cp.async`/predicated loads. Either way the K=511 case cannot produce wrong
  numerics; it produces a plan decision with a reason, or a located error.
- GOLDEN gates make the guarantee measurable: the gathered-GEMM golden with
  K=511 is the regression that must stay exact under every lowering the
  planner may choose.

Design and staging for the machinery this case motivates: `docs/tma-gather.md`.
