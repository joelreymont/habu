---
title: Finish the x86 native body-reuse ABI
status: active
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.358152+03:00"
---

Completed native feature rvmtmsrkb9f7f972, pushed intel-bodyabi; rebased integration candidate140a8c27 over profiler/BP/throws. Independent review found no findings. Cold0hits/0skips/7compiled -> warm7hits/7skips/0compiled; native build, gen5/gen4 byte identity, forms/state/admission/cache/data/seam and native-build-edit digest test pass. Integration build succeeded after catalog/layout-only stage; focused integrated tests running. Mechanical tests: x64-emit-fixture,x64-regalloc,x64ir and both x86 harnesses; native-unit-file adds x86 format refusal/roundtrip. OPEN Mac ARM proof and integrated full native suite. Replay session errors belong fixed-definer lane; auto byte differences belong image-state; seeded true contracts belong image boot fix.
