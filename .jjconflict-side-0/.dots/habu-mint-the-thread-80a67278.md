---
title: Mint the thread-invariant dictionary origin
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T09:54:42.235600+02:00"
---

Found by the does-conv lane: does>-created words publish the ABSOLUTE address of dictionary storage (same in every thread); generated data-base <off> + accessors resolve against register 20, which a task's thread entry points at the task's own 64KiB region - inside a task the two name different objects (measured: converting FACILITY made every task lock its own uninitialised mutex, E-TASK-THREAD; a probe faulted at region-base + dictionary-offset). Register 26 (dbase@) already holds the thread-invariant base but is typed -- n. Mint ONE primitive: a ( -- ptr a ) thread-invariant origin word with its checker axiom. It unblocks the four remaining does> conversions (BUFFER:, BUFFER-E, TASK, FACILITY) AND retires the layout-buffer family's SILENT per-thread property (~388 generated data-base sites whose task-reachability is unmeasured - the same hazard, pre-existing). Files: src/habu (primitive), src/core/checker.f (axiom). Depends: none.
