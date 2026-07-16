---
title: Device re-run of typed vec4 path on zed
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T23:16:28.385204+02:00"
blocks:
  - habu-infra-zed-unreachable-c3d8c991
---

Residual from habu-ptx-m10-vectorization-f394cfe1 (2026-07-15): the typed .V4 surface (lib/ptx/tile-v4a.f) landed with device correctness established via the byte-identity bridge - typed SAXPY-V4A emits PTX byte-for-byte identical to the hyphenated SAXPY-V4 that maki/onnx-era saxpy-v4-tail-device-test.f validates on-device (body + masked tail, n=4,5,7,1000003, poison-readback probe). A LIVE device run through the typed path itself is still owed. BLOCKED on zed access: ssh now fails with 'Tailscale SSH requires an additional check' (interactive re-auth the user must complete). When zed returns: run the typed kernels through the standard isolated-/tmp-root flow, verify golden PASS + probe FAIL, done. Files: none expected (evidence-only) unless the run finds a divergence. Ownership: ptx typed-DSL device evidence. USER-GATED until Tailscale re-auth.

QUEUED 2026-07-16 (GEMM stage-3 landing, 4a5d876a): on zed's return also
re-run tools/ptx/device-gold.f GEMM-GOLDEN and the maki eval device fixtures
(eval-emit-device-test) against the now-checked production EMIT-MATMUL -
byte-identical PTX to the previously device-proven golden, so expect V-PASS;
this is the formal on-device discharge of habu-re-express-tiled-9cc4a73a's
final acceptance (closed on the byte-identity bridge, M10 precedent).
