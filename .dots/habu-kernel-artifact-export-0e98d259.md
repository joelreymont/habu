---
title: Kernel artifact export for consumers
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:32:21.894725+02:00"
---

No packaging story: habu runs only from its repo tree via bin/hb; an external project (odin, Zig build) cannot depend on checked-Habu output. Kernel roadmap explicitly targets running inside odin's ZED CUDA context (docs/ptx-sketch.md:345,375) yet there is no .ptx artifact export, no manifest (kernel name, param ABI span->(base,len,align), launch geometry, sm target, content hash), no build-step integration. Fix: 'hb kernel-export' emitting PTX + JSON manifest as versioned artifacts, ABI documented as a contract (ptx-sketch.md:361-375), example Zig consumer wiring in odin. Prereq for any real habu-as-dependency use.
