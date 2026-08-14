---
title: "Fix the engine's wide-instantiation load guard"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T08:20:40.356763+02:00"
---

PRIORITY 1 - ENGINE CRASH ON VALID DATA, found by the wide-load lane (40b81d51), reproduces on MASTER with no migration involved: the engine cannot read a parametric family instantiated wider than its declaration back out of a TYPED-BUFFER - its own STORE writes correctly, its LOAD aborts hb: bad layout tag exit 85. Cause: src/core/layout-valid.f QUEUE-SUM takes the tag's slot from the family's DECLARED slot count (fam TFAM-SLOTS@), which at a wider instantiation is a payload cell - the guard tests the wrong cell against the tag domain. Twenty-line reproducer in test/compiler/native-wide-mem.f WIDE-INST-CASE prose. The chain's wide load is more correct than the engine's for this shape (that case's second column is the chain's own load, stated in the file). Fix: the guard takes the INSTANTIATED slot count; regression executes the reproducer both ways. Files: src/core/layout-valid.f. Depends: none.
