---
title: A poll window of 2^31 ms or more is cut short or never ends on macOS
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:53:05.620575+02:00"
---

Problem: BPOLL (src/habu/habu1.f:959) passes a 64-bit ms count to poll(2)'s C int on macOS (:1001-1009); Linux converts it (:983-999). '1 4294967306 >MS PROCESS:POLL' times out in 11 ms, and 2^31 to 2^32-1 ms waits forever. habu1.f is machinery being replaced (docs/design.md "Machinery being replaced"), so the engine word is not patched: this is an acceptance case of the one typed interpreter; until then a caller-side cap such as src/lib/pg.f WAIT-MAX (:145) is the only bound. Acceptance: a window of 2^31 ms or more waits its time or is refused, the same on macOS and Linux. Depends: none. Claim: unassigned.
