---
title: "Test optimization: cut native gate wall time"
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:57:55.711568+02:00"
---

Campaign only; do not dispatch this parent. The native gate (bin/hb --load test/run.f) took 787 s wall on an idle host: 99 s of serial setup (15 s cold engine, 84 s whitebox native build, test/gate-stdlib-lib.f:108-113), then a pool phase of 688 s against an 8-slot ideal of 456 s for 3647 s of row work. The pool starts rows in registry order and each waits for a free slot (test/gate-pool.f:1214-1222), so a pool barrier or a late long row idles slots. Measured in native-gate.log (491/492) and native-gate-serial-floor.log (3 TIMEOUT-UNDER-LOAD rows killed at the 360 s deadline under external load). The speed work removes the barriers, orders the rows longest first, overlaps the whitebox build with the pool and keeps each row under half its deadline. Close this parent when the speed work has landed and a green gate shows the reduced wall time.

Later source, diagnostic and runtime defect dots were filed inside this
directory, but are independent of gate wall time. Their open status does not
block closing this campaign; each keeps its own acceptance.
