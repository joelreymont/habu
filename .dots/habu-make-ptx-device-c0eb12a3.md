---
title: Make PTX device proofs fail closed
status: open
priority: 1
issue-type: task
created-at: "\"2026-06-27T15:32:50.869667+02:00\""
---

Deep-review finding 2026-06-27: tools/ptx/cuda-launch.f, cuda-load.f, bandwidth.f, softmax-gradcheck.f and maki/eval-device*.f drop CUDA Driver rc values, reuse output buffers, use fixed /tmp grader paths, and sometimes print NO then exit success. Correct fix: add shared checked CUDA symbol/rc wrappers with named errors; reset per-run state; initialize readback sentinels; check every DLOPEN/DLSYM/CALL rc; free device allocations with cuMemFree_v2; use private TMPDIR/HB_TMP roots for grader driver/PTX/cubin; resolve PTXAS from env; check nonuniform multi-element goldens; wire a hardware/device gate distinct from CPU-only maki checks. Verify stale/missing cubin, missing symbol, failed ptxas, failed launch, failed readback, and wrong candidate each fail in the correct class.

2026-07-03 increment: `maki/cuda-types.f` now owns shared fail-closed
`CUDA-HANDLE0` / `CUDA-RC0`; `maki/gpu.f`, `maki/eval-device.f`, and
`maki/eval-device-sm.f` check `DLOPEN` and every CUDA rc through those wrappers,
and free device allocations with typed `cuMemFree_v2` bindings before unloading
modules/releasing primary contexts. Remaining: private temp roots/PTXAS env,
sentinel readbacks, zed hardware failure-class tests, and nonuniform goldens.

2026-07-03 increment: grader artifact paths no longer use shared
`/tmp/grade*` names. `maki/device-artifacts.f` owns per-grade `TMPDIR` roots,
driver/PTX/cubin paths, cleanup, and `PTXAS` env resolution with the existing
CUDA default fallback. `maki/eval-device.f` and `maki/eval-device-sm.f` now
require their own stdlib/PTX dependencies and use those artifact paths; focused
artifact tests and the full native suite pass. Remaining: sentinel readbacks,
zed hardware failure-class tests, nonuniform goldens, and migration of older
`tools/ptx/*device*` hardcoded ptxas/tmp paths.

2026-07-03 increment: `lib/ptx/toolchain.f` now owns private PTX/cubin
artifact roots for reusable PTX device tools plus `PTXAS` env/default
resolution and the checked assembler runner. `tools/ptx/acc-device-test.f`,
`redadd-device-test.f`, `matmul-device-test.f`, `gradcheck.f`,
`indexed-scatter-gradcheck.f`, `scatter-add-gradcheck.f`, and
`saxpy-v4-tail-device-test.f` use that helper instead of fixed
`/tmp/*.ptx`/`/tmp/*.cubin` paths or hardcoded ptxas argv. Mac proof: native
suite green and `maki/test.f` green. Remaining: sentinel readbacks, zed
hardware failure-class tests, nonuniform goldens, and older prebuilt-cubin
consumer tools (`cuda-load`, `cuda-launch`, bandwidth/fusion/softmax launchers).

2026-07-04 increment: sentinel readbacks landed. New `lib/ptx/sentinel.f`
(`PTXSENT`) owns the committed poison pattern (`$DEADBEEF`), `FILL` (pre-launch),
and `GUARD` (throws `E-PTX-READBACK` = -5003 when a readback cell is still the
sentinel), with `lib/ptx/sentinel-test.f` in the `ptx-stdlib` gate. Every
golden-readback host buffer is now poisoned before launch and guarded on read:
`maki/gpu.f` (GHY), `maki/eval-device.f` (ED-RBUF), `maki/eval-device-sm.f`
(SM-OUT), and `tools/ptx/{acc,matmul,redadd,saxpy-v4-tail,gradcheck}-device*`,
`sum-launch`, `softmax-launch`, `softmax-gradcheck`. A dropped copy-back now fails
with a named class instead of comparing poison as a wrong answer. Migration:
`tools/ptx/cuda-load.f` deleted (dead legacy - superseded by `acc-device-test.f`
and `saxpy-v4-tail-device-test.f`, which load AND launch AND assert SAXPY);
`sum-launch.f` and `softmax-launch.f` migrated to self-contained per-run `PTXTC`
emit+assemble (no shared `/tmp/{sum,softmax}.cubin`). Goldens: `sum-launch.f`
row1 changed from uniform `[1,1,1,1]->4.0` to nonuniform `[2,3,4,5]->14.0`
(0x41600000, hand-verified `14.0 F64>F32 = $41600000`) so an index/broadcast bug
cannot pass on all-equal data. Proof: `maki/test.f` 60/60, full native gate green
(incl. `ptx-stdlib`, host-lint, filemap-lint), touched device files checker-clean
(device-blocked off-device). Remaining (device-blocked / dotted): zed hardware
failure-class tests; multi-cubin consumer migration + toolchain named-artifact
capability (`softmax-gradcheck.f` keeps fwd+bwd loaded simultaneously); bench
harness self-emit for `fusion-compare.f`/`bandwidth-lib.f` (PTXBENCH has no emit
path); `maki/gpu.f` `/tmp/saxpy.cubin` self-emit; `matmul-device-test.f`
references undefined `ED-LIB`/`ED-H`/`ED-SYM` (pre-existing, cannot load) and its
A=B=all-ones golden cannot catch a transpose; `softmax-launch.f` row1 stays
uniform (its ex2.approx softmax bits need device measurement, not hand
computation - row0 `[1,2,3,4]` already gives index discrimination);
`cuda-launch.f` deferred to the f32-marshaling lane.

2026-07-04 zed-WIP disposition: the rescued branch zed-wip-cuda-driver
(a0f31639, 13 files, written Jul 1 on-device against this dot) was analyzed
against fable post-completion: 100% superseded/dead/conflicting (untyped
CALLn-RC driver loses to maki/cuda-driver.f typed FFI: bindings; its PTXTOOL
loses to PTXTC; its error codes collide with E-PTX-EMIT -3412; cuda-load.f
target deleted). Zero commits salvaged; the one sound idea became dot
habu-ptx-promote-checked (promote typed bindings to lib/ptx). Branch is
retire-recommended (kept on origin pending user nod to delete).
