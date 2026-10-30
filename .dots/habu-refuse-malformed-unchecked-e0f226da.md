---
title: Refuse malformed unchecked bodies in the JIT
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T09:31:53.185764+02:00"
---

Census B (/tmp/jerry-big/t0cmp-B.md, probes /tmp/jerry-t0cmp-B/): the JIT alone accepts else-twice (wrong answer: 1 S prints 3), j-one-level (call dies rc 102), leave-in-quot2 (rc 102), and under 0 set-check a does> clause's recurse runs the definer (rc 78). T0 and tier 1 refuse each.

Census A probe c-under (/tmp/jerry-t0cmp-A/p-c-under-*): a TRUSTED: or 0 set-check construct whose call takes more cells than the definition declares compiles under T0 as under the JIT; tier 1 ends rc 67 (-8304). Same class as the depth faults above: T0 and the JIT both need the refusal.
