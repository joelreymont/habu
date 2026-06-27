---
title: "A: checked prelude words"
status: closed
priority: 1
issue-type: task
created-at: "2026-06-27T13:15:32.692129+02:00"
closed-at: "2026-06-27T13:30:20.589033+02:00"
close-reason: Landed lib/prelude.f (true/false/0<>/fdrop) on habu master, gate-green. f<=/f>=/fdup/fover deferred -> follow-up dot.
---

Bucket A (trivial, safe, lib-only). Add a checked prelude (lib/prelude.f or extend lib/float.f): true (0 0=), false (0 0= 0=), 0<> (0 <>), fdrop (drop typed for r), f<= (f> 0=), f>= (f< 0=), f= , fdup/fover. Type them so the checker sees bool/r, not generic n. Removes ~3 round-trips/module. Register on master like lib/hashmap.f (manifest+FILEMAP+gate+lints+fixpoint-green).
