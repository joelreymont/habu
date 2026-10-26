---
title: Tier 1 refuses loops that swap their values
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T20:21:45.576890+02:00"
---

Found by lane 1a (jerry, 2026-10-26). At tier 1, `: GEJSWAP ( n -- n n ) dup 1+ begin swap 1- dup 0= until ;` dies 'ncomp: cannot compile GEJSWAP', throw -8507 E-A64RA-EDGE (src/compiler/native/regalloc.f:1451). The float pair swap loop GEJFSW does the same. Reproducer: ~/.cache/tmp/jerry-t1swap.f with `~/.cache/tmp/jerry-host/hb --load`. Executables build at tier 1, so they hit it too. GE-VJOIN-AT (test/runtime-regression-test.f) runs GEJSWAP/GEJFSW at tier 0 only; drop that tier gate when this is fixed. Weigh against tier 1 retirement before fixing.
