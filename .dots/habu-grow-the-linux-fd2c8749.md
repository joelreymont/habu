---
title: Grow the Linux AIO ring with its records
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T22:55:00.021808+02:00"
---

Joel 2026-10-28 (interview): grow the io_uring rings in place with IORING_REGISTER_RESIZE_RINGS (Linux 6.13+) as records grow, and raise aio.f's Linux floor from 5.10 to 6.13; no new public AIO word (no AIO:RESERVE). Today src/lib/aio.f sizes the completion ring at URING-SETUP from AIO-CQ-ENTRIES $200, and URING-CLAIM refuses with E-AIO-FULL once REC-LIVE reaches CQ-ENTRIES-N, so a server such as WS (two records per socket plus its wake) fails mid-serve. Builds on phil's aio growth lane zwprsyuz 747f1227 (records grow on demand); lands after it. The record-count E-AIO-FULL goes away; update aio.f's header (5.10), its tests and docs. Asked by tim for the WS stack (tim-ws), which waits for this instead of a per-op E-AIO-FULL catch. x86-64 proof host pulsar-ua-106 runs 6.18.

Brief .jj-ws.noindex/carl-ring/.out/brief-ring.md, workspace on 747f1227. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-ring.
