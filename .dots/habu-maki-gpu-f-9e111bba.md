---
title: maki gpu.f retire /tmp/saxpy.cubin
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:09:34.628675+02:00"
---

maki/gpu.f G-SETUP loads a prebuilt /tmp/saxpy.cubin (maki tensor SGD lowering onto the checked SAXPY kernel). Migrate to self-contained emit: spawn bin/hb to emit tools/ptx/saxpy-cg.f (SAXPY emit prelude: lib/errors lib/string lib/float lib/fmt src/arch/ptx/emit lib/ptx/cg lib/ptx/header lib/ptx/cg-collective lib/ptx/tile lib/ptx/collective) to a private PTXTC (or MAKI-GRADE) root, assemble, load PTXTC:CUBIN$, retire the /tmp name. gpu.f is loaded by the maki gate as a lib (G-SETUP/G-LAUNCH run only on device), so keep maki/test.f 60/60. Sentinel readback (GHY FILL/GUARD) already added. Device-blocked to run (Orin).
