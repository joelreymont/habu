---
title: Fix the codegen-compare pool-only crash
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T11:40:03.422424+02:00"
---

test/run.f red: tools/codegen-compare-test.f and tools/codegen-compare-clang-test.f exit 134 when run under the gate pool but pass standalone (rc=0). Observed on master 10281011 during the device-suites lane's pristine-parent control. Suspect territory: the clang column's process-global state under the pool's fork model — CODEGEN-CC builds the reference in a per-process temp dir (TMPDIR-MKDIR habu-ccref) and dlopens the dylib; a forked pool member may inherit or race another member's store, or REMOVE may tear down a tree a sibling still maps. Timing of first appearance unknown: the tip-green worker saw 8 reds before the measurement merge (926eab55), this set has 11 — bisect whether 926eab55 introduced it. This is codegen-owned. Reproduce under the pool, attribute exactly, fix the root cause (likely: reference build must be per-member or pre-fork, decided by ownership not luck), regression test in the pool configuration that failed.
Claim: agent=pool-crash workspace=.jj-ws/habu-fix-the-codegen-224669b6
