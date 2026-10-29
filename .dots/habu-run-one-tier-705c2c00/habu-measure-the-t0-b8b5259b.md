---
title: Measure the T0 line tip against the JIT
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T15:09:17.468880+02:00"
closed-at: "2026-10-29T15:09:17.519100+02:00"
close-reason: measured
---

/tmp/jerry-meas/line1, 7 interleaved zz rounds, medians, instructions retired: JIT load 3.0518G run 12.7660G; line tip (pwxnuopw, jerry-t0line/bin/hb) load 3.5053G (1.149x) run 12.5707G (0.985x). Program output byte-equal to the JIT's once T0's driver line 't0: bodies 789 tier 1' is dropped. Load figure is gross, driver included.
