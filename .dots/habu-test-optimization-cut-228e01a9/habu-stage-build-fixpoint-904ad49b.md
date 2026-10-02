---
title: "Stage build-fixpoint's engine through RESERVE-SIBLING"
status: open
priority: 4
issue-type: task
created-at: "2026-10-02T08:52:24.902538+02:00"
---

Problem: tools/build-fixpoint.f:1765-1770 and :1832-1839 BF-INSTALL-HB stage the engine at a fixed non-exclusive '<engine>.tmp' sibling and clean it behind an EXISTS? check: a third sibling-naming implementation beside lib/fs-mutate.f RESERVE-SIBLING, which the image writer, ATOMIC-WRITE-FILE and (r4-hbbinst 2c502a11, dot 0d2dc1d6) hb-build's install share. Found by the r4-hbbinst lane. Acceptance: BF-INSTALL-HB stages through RESERVE-SIBLING the way 2c502a11's HBB-INSTALL-STAGED does; the fixed name and EXISTS? guard go; a failed install leaves the engine as it was and no sibling (tools/build-fixpoint-test.f case). Base: after 2c502a11 lands. Also in hb-build: an -o path near the span limit now throws E-SPAN-CAPACITY from RESERVE-SIBLING (was E-BUILD-PATH) and the worker reports both end uncaught in the CLI: reproduce, and if the CLI does not report it by name, report it (case in tools/hb-build-cli-errors-test.f).
