---
title: Keep gate-pool red detail beyond sixteen
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:58:29.576130+02:00"
---

Full context: GT-POOL-RED+ (test/gate-pool.f:821-825) stores detail for only the first GT-POOL-RED-MAX (16) reds; later reds are counted but lose their captured stdout and stderr paths, so a wide breakage cannot be triaged from the gate output. Separately, document that placing any gate-stdlib-cases.f suite in a SEQ group makes GSI-LOAD-FINISH (test/gate-stdlib-inline-lib.f:137) throw and abandon every later suite in that phase — no suite is sequential today, so nothing is masked now, but adding one would silently reintroduce whole-phase fail-fast. Fix by spilling red detail to files instead of a fixed table, and add a gate-pool test asserting every red is reported when a phase produces more than sixteen.
