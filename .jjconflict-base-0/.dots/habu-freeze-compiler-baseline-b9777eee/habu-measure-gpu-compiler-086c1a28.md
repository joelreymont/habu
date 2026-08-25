---
title: Measure GPU compiler baseline
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:49.559678+02:00"
---

Full context: design sections 14.2 and 16.5 require current generated-kernel evidence before GPU lowering changes; the existing opt-ir dot owns only optimizer adjudication. Record PTX bytes/instructions, ptxas time, cubin size, registers, shared memory, spills, occupancy, traffic, device time, throughput, and roofline class for pinned representative kernels. Acceptance: every row binds device, target, toolchain, source digest, protocol, correctness result, and raw artifacts; missing Spark access blocks this dot only.

Blocker sweep 2026-08-21 (tracker GC): the blocks: list is gone because every entry in it was already closed - habu-adjudicate-dormant-ptx-482310bc. The prose above still names them as prerequisites; they are satisfied, and nothing in the tracker blocks this leaf now.
