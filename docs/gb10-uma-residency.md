# GB10 UMA weight-residency measurement

Deciding evidence for the inference engine's weight-residency policy
(dot `habu-infer-safetensors-loader-0b58e06a`, plan of record
`docs/inference-engine-plan.md` M1). Produced by the sanctioned timing lane
`maki/infer/residency-probe.f` + `maki/infer/resid-kernel.cu` on the idle GB10
(`spark`, driver 580, CUDA 13.0, `sm_121`, ~273 GB/s LPDDR5X shared CPU/GPU,
121 GiB usable unified pool). Direct mmap was treated as an **option, not a
predetermined policy**; four options were compared fairly.

## Method

One grid-stride read-reduction kernel (`resid_read`) streams 100 MiB of `float4`
loads once per launch, reduces per block through shared memory, and folds one
`atomicAdd` per block into the output — coalesced 128-bit loads, memory-bandwidth
bound (not atomic bound). The **only** thing that differs between options is the
input pointer. Bandwidth = bytes / GPU time.

- **Steady-state**: `PTXBENCH:BENCH-GPU-NS` runs one full grid-stride **warmup
  launch** (faulting every page in via ATS) before a 50-iteration timed burst;
  CUDA-event timed.
- **Cold first-touch**: a single **un-warmed** launch, host-clock timed, capturing
  the ATS page-fault-in cost. The 100 MiB file is page-cache-resident (written
  this session), so cold reflects GPU-side ATS mapping establishment, not disk I/O
  (measuring disk-cold would need `drop_caches`, which needs root).
- Device resources are held in an exception-safe `CUDA-SCOPE` frame.

`cuMemHostRegister` with `CU_MEMHOSTREGISTER_READ_ONLY` (0x08) returns
`CUDA_ERROR_NOT_SUPPORTED` (801) on this platform; `DEVICEMAP` (0x02) succeeds.
`cuMemAdvise` applies to `cuMemAllocManaged` unified memory, not to a file
mmap + register, so it is out of scope for the mmap path.

## Results (100 MiB, representative of 3 runs)

Steady-state GPU read bandwidth (prefaulted):

| Option | GB/s | % of device |
|--------|-----:|------------:|
| (3) `cuMemAlloc` device buffer (copied once from the file) | **273–279** | 100% |
| (2) mmap + `cuMemHostRegister` (DEVICEMAP), read via device ptr | 160–161 | ~58% |
| (1) file-backed mmap read directly (bare host pointer, ATS) | 157–161 | ~58% |

One-time costs (per 100 MiB):

| Op | effective GB/s | time |
|----|---------------:|-----:|
| (3) HtoD populate the device buffer | ~60 | ~1.7 ms |
| (1) mmap direct **COLD first-touch** (ATS page-fault-in) | ~2.6–3.2 | **~32–40 ms** |

## What the numbers say

1. **A `cuMemAlloc` device buffer saturates the pool (~273–279 GB/s); a directly
   mmap'd host pointer tops out at ~58% (~160 GB/s).** Even on this fully coherent
   UMA part, ATS-translated system-memory access does not reach the bandwidth a
   native device allocation gets (device page tables / placement matter, not the
   physical location — both are the same LPDDR5X).
2. **`cuMemHostRegister` does NOT recover the bandwidth** (160 vs 160). Registering
   with DEVICEMAP hands back a device pointer but the access still goes through the
   ATS/system path. So the lever is a real device *copy*, not registration.
3. **The one-time copy is cheap: ~1.7 ms / 100 MiB (~60 GB/s HtoD).** For a 500 MiB
   model that is ~8.5 ms one-time, amortized over the entire inference session.
4. **Cold first-touch of an mmap'd region is catastrophic: ~30–40 ms / 100 MiB
   (~100x the steady 0.35 ms/pass).** ATS page-fault-in must NEVER happen on the
   decode path.

## Policy recommendation (differs by model size)

Decode is bandwidth-bound; every weight is re-read every token. Paying a one-time
~1.7 ms/100 MiB copy to buy ~1.75x steady read bandwidth (279 vs 160 GB/s) forever
is decisively worth it whenever the copy fits.

- **Model + KV cache + activations fit in the 121 GiB pool with headroom
  (GPT-2 124M; LLaMA-7B ≈14 GiB bf16; anything NVFP4-quantized): COPY ONCE into
  `cuMemAlloc` device buffers.** The safetensors loader mmaps the checkpoint
  zero-copy; the residency layer copies each tensor into a device buffer, then
  **unmaps the source** (`SAFET:UNMAP`) so two full checkpoints are never resident
  at once. Peak load memory = model size + one in-flight tensor, bounded and small.
- **Model at the memory ceiling where a full device copy + KV cache will not fit
  (e.g. 70B bf16 ≈140 GiB > 121 GiB): do NOT copy-once.** Either quantize first
  (NVFP4, epic phase 4 — the real lever) or serve weights from the direct mmap and
  accept ~58% bandwidth on the tensors that do not fit. Never rely on the 16 GiB
  swap file for weights — the epic's premise is that UMA deletes the swap tier.
- **Always prefault** any mmap'd region that the decode path will touch (a warmup
  read pass) — never take the ~30 ms/100 MiB cold ATS fault mid-decode.
- **(2) `cuMemHostRegister` is not useful here** (no bandwidth gain over bare mmap);
  the bindings landed (`CU-MEM-HOST-REGISTER` / `-GET-DEVICE-POINTER` /
  `-UNREGISTER`) remain available should a managed-memory `cuMemAdvise` path be
  measured later.

## Memory accounting discipline (residency layer contract)

- **Never two complete checkpoints resident simultaneously.** Copy-once unmaps the
  source mapping after conversion; peak loading memory is bounded (model size + one
  tensor) and must be measured per model.
- **Report headroom at load** and refuse to over-commit the pool. Measured baseline
  on `spark`: 121 GiB total unified, ~118 GiB available idle, 45 GiB reclaimable
  buff/cache, 16 GiB swap (unused, priority -2), THP = madvise.

## Forward note: packed layout (not measured here)

A **packed** (pre-transposed and/or NVFP4-quantized) on-device layout is a later
candidate (model-pack dot). The safetensors loader records tensors in their
**original** on-disk orientation (HF GPT-2 Conv1D weights are `[in, out]`, the
transpose of `nn.Linear` `[out, in]`); a packing pass would materialise the
Linear-oriented / quantized form once, at which point copy-once and packing fuse
into a single device-resident conversion. Packing does not change the residency
conclusion above — it only reduces the bytes that must be streamed per token.
