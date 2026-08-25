---
title: "Fix DRAM-roof probe: N=2^20 is L2-resident on GB10"
status: closed
priority: 2
issue-type: task
created-at: "2026-07-19T00:29:06.853072+02:00"
closed-at: "2026-07-19T03:02:56.889130+02:00"
---

Codegen-verdict follow-up split out of the closed toolchain dot: the bandwidth probe (maki bandwidth-lib.f) uses N=2^20 floats, which fits the GB10's L2 and measures 1516 GB/s - not DRAM. A true DRAM roof needs a working set well past L2 (N >= 64M elements) and should size itself from the probed device (the active-target descriptor now exists), not a constant. Matters for any roofline claim the GB10 head-to-head makes (dot habu-gb10-gemm-head-9ea0290b depends on honest roof numbers).
