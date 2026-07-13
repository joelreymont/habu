---
title: "maki: sched-key-test leaves the SK table full (suite hygiene)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T11:00:25.367719+02:00"
---

Replay-wiring residual (2026-07-13): maki/sched-key-test.f fills the SK replay table and does not reset it; maki/test.f is one process, so any LATER suite whose production path now SK-PUTs (cad-test PROMOTE does since b2a7f384) can hit E-SK-FULL depending on suite order. cad-test defends with a hermetic prelude (SK-TAB-RESET); fix the root: sched-key-test resets the table at its tail (and any other suite that fills shared maki singletons audits the same - one sweep). Small.
