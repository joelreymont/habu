---
title: A fully re-emitted class leaves its definition as dead code
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T20:55:01.017707+02:00"
---

Residue from the remat landing (spillclose lane 2026-08-12): when every read of a class is re-emitted in place, the original defining movz stays in the module as dead code - correct output, wasted bytes (one instruction per fully-rematted class). Fold or dead-strip it: the combine pass's existing single-use machinery is the likely home (a definition with zero remaining readers after the spill rewrite). Measure the corpus byte delta; the judge artifact re-pins with the derivation. Files: src/compiler/native/{combine,spill}.f. Depends: the remat landing merging.
