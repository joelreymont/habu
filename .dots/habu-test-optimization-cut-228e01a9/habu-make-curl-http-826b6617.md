---
title: Make curl-http request count independent of scheduling
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T15:56:11.361329+02:00"
closed-at: "2026-09-30T15:56:11.369046+02:00"
close-reason: "Landed with this commit (lib/net/curl-test.f, docs/curl.md). CASE-HALTED waits for its request (WAIT-HIT, 2 s) before halting; TEST-TIMEOUT and the stalled transfer use a listener nothing accepts on; DRIBBLE-CEILING measures its arrival after a serial-server barrier and TEST-SERVER asserts 54 plus it. Before: 0/10 at each injected loop delay (8, 20, 120 ms), a 4 s freeze failed at 4 of 12 offsets; after: 10/10 each, 12/12, 20/20 at load 108-117, 39/40 at background priority (the one failure a 71 s stall past the row's 10 s REQUEST-MS ceiling, same in the old row). Mutations caught. Fable review: accept; its one doc clause applied. Row 3/3 at 3.2 s, load 9, on master 3be2e70e."
---

Problem: gate row curl-http (lib/net/curl-test.f) asserted a fixed count of requests reaching its test server and failed with expected 56 got 55 under load: CASE-HALTED lost its request when the CURL loop answered START 8 ms or more after a 5 ms settle sleep; CASE-STALLED lost it when a loop turn ran 100 ms late (STALL-MS cut 500 -> 100 in round 1); TEST-TIMEOUT and the stalled transfer also touched the count; DRIBBLE-CEILING's request may or may not arrive. Acceptance: the outcome is independent of scheduling delay within the row's documented bounds; every detection kept; DRIBBLE-CEILING's ceiling claim tested whenever the dribble started; no retry loops. Verify: injected loop latency and whole-process freezes before and after; mutations; plain bin/hb --load lib/net/curl-test.f.
