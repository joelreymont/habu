---
title: "Gate: report timeout kills as timeout-under-load"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T18:51:33.316966+02:00\""
---

From the catchframe integration RCA (2026-07-16): pool-timeout SIGKILLs (rc 137) of gate children whose logs show partial SUCCESS (e.g. 'hb-build OK: .../hb-aot-preseed' printed before the kill, WAIT lines at 5097ms/10109ms showing pool saturation) are reported as plain FAIL/RED, indistinguishable from real failures - this cost a four-phase misattribution on a contended host. Fix in the gate harness (test/gate-pool.f / run-lib reporting): when a slot is killed by the pool's own timeout/reaper (the harness KNOWS it sent the SIGKILL - distinguish from a child that died 137 on its own), report the phase as 'TIMEOUT-UNDER-LOAD' (still red/failing exit - a timeout is not a pass - but attributably distinct in the RED: line and gate-stats), including the slot's WAIT/saturation stats. Acceptance: a fixture with a deliberately slow child under a tiny timeout produces the distinct verdict token; real child crashes with rc 137 not sent by the pool still report FAIL; run.f output format change is reflected in any parser that consumes RED: lines (rg consumers first); full run.f green. Files: test/gate-pool.f, run-lib reporters, focused fixture. Ownership: test harness attribution.

Claim: agent=gtimeout workspace=.jj-ws/fable-gtimeout
