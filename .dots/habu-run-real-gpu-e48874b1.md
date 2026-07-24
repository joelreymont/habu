---
title: Run real GPU fault gate last
status: open
priority: 2
issue-type: task
created-at: "2026-07-24T03:53:05.417002+02:00"
blocks:
  - habu-end-gpu-fault-b894476c
---

Why: maki/eval/device-fault-test.f causes a real NVIDIA Xid 31 memory fault. It currently runs inside maki/test-eval.f while the four Maki slices and other candidate GPU work overlap. The full gate then assumes the same device is immediately reusable. A destructive hardware qualification must be terminal and exclusive.

Owners: test/run-lib.f and test/run-resident.f own gate scheduling; tools/suite-coverage-lint-core.f owns exact Maki suite membership; package TEST owns the loaders. No inference or grader API changes.

Dependency: after habu-end-gpu-fault-b894476c, whose suite makes the fault its final CUDA operation.

Exact result: add maki/test-device-fault.f as a dedicated standalone loader containing only maki/eval/device-fault-test.f. Remove that suite from maki/test-eval.f. Keep the four ordinary Maki slices parallel. After candidate build readiness, every ordinary Maki slice and every remaining gate phase must finish and the process pool must have zero live workers. Only when the accumulated red count is zero may the runner start the dedicated fault loader against the exact candidate engine with its own HB_TMP, standard environment, timeout, statistics, and fail-closed result handling; drain it before completion. If any earlier phase is red, do not trigger the destructive fault. In maki/test.f, retain the suite exactly once and move it to the final suite position so the standalone run has no later CUDA work. Extend the structural suite-coverage owner so the new loader is canonical, the fault suite occurs in that loader only, all master members still occur in exactly one loader, comments or strings cannot satisfy membership, and the terminal loader cannot enter the ordinary parallel loop. Use named slice indices/counts and reject an out-of-range slice instead of defaulting to a different loader.

Forbidden: retry, sleep, device reset, health polling, mock fault, threshold changes, skipping the real fault on DGX Spark, running it concurrently, running later CUDA work, treating a red prior phase as permission to fault, a second manifest, substring counting, or a shell wrapper.

Pre-change production failure: repeated bin/hb --load maki/test.f runs reached the post-fault known-good candidate with expected EVN-GREEN 3 but got EVN-DEVICE-FAULT 4. The gate scheduler launches device-fault-test.f as part of the eval slice alongside the other three slices and remaining candidate phases.

Acceptance: scheduler tests must prove the terminal loader cannot start while a pool worker is live, after any prior red, or through the ordinary four-slice launch loop; it starts exactly once only after a clean drain and its failure contributes a red phase. Structural hostile fixtures must prove comments, strings, duplicate entries, wrong loaders, and reordering cannot satisfy the terminal membership rule. On DGX Spark, the exact full gate must show all normal phases complete before the announced Xid 31, then run only the fault loader, report its verdict, and perform no later GPU work. The standalone maki/test.f must place the fault suite last. Run focused runner, pool, suite-coverage, device-fault, Maki, typed-local diff, package diff, trust, host, filemap, dot, and full native gates on the exact candidate tree.
