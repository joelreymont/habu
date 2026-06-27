---
title: "Codegen: lower checked tile-IR to PTX (connect the halves)"
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T08:06:44.218989+02:00"
blocks:
  - habu-ptx-m4-tile-6a825f56
---

CRITICAL gap #1. The checked tile vocabulary (lib/ptx-tile.f, lib/ptx-collective.f: LOAD/STORE/SCALE/+./collectives) are TRUSTED: stubs that throw E-PTX-NOIMPL - they type-check but do NOT emit PTX. The only real PTX comes from src/arch/ptx/emit.f (a hardcoded SAXPY string printer). So checked-SAXPY and the SAXPY that ran on the GPU are TWO different kernels. Build the codegen that lowers a checked tile kernel body (the typed IR word list) to PTX, replacing the stub bodies with real emission (mask predication, coalesced ld/st.global, the collective shfl+shared lowering). Then a CHECKED kernel emits -> ptxas -> runs on GPU (proven path, tools/ptx/cuda-launch.f). This is M4e generalized + M5/M6 lowering.
- Files: src/arch/ptx/ emit + a per-op lowering; wire to KERNEL: bodies.
- Verify: checked SAXPY (lib/ptx-tile-test.f) emits PTX that ptxas assembles and launches to the same golden as the hand-built emit.f. Then SOFTMAX-ROWS.
- Dep: M4 types (done). THE highest-leverage build - it makes checked kernels run on GPU end to end.
