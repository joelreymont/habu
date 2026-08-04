---
title: Migrate remaining PTX tools to CUDA scope
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:05:46.835053+02:00"
---

The earlier CUDA-scope migration closed after thirteen callers, but newer and remaining PTX tools still use happy-path ownership. In particular tools/ptx/layernorm-device-test.f retains a context, loads a module, allocates RN-dX/RN-dDY/RN-dO, never frees those buffers, and has no partial-setup unwind. Re-census every production PTX tool added or omitted after commit 771d921a and migrate each fallible acquisition immediately into the landed CUDA-SCOPE owner. Include device buffers, modules, contexts, events, PTX toolchain temporary roots, and any host mapping owned by the tool; remote scratch stays under habu-run-remote-gpu-b523f6b2 and inference residency mappings under habu-own-residency-probe-5ea0a142. Add injected failure after every acquisition and operation, proving zero outstanding resources, reverse-order exactly-once release, original primary error, retained cleanup errors, and a clean successful rerun. For layernorm specifically prove all three buffers, module, and context release on every path. Preserve numerical/device results and timing outside cleanup. Files: only the current residual PTX tools and focused lifecycle tests. Verify host-injected matrix, live GB10 layernorm golden, PTX standard library, package/host/dot lints, and full native gate.
