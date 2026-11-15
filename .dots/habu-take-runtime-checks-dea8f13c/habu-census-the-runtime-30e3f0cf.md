---
title: Census the runtime checks default code runs
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T10:21:15.620604+02:00"
closed-at: "2026-11-13T10:28:03.872758+02:00"
---

Problem: the design that takes runtime checks out of default code needs every class of check default code runs, where it is emitted, what program error it catches and what a static check would need. Acceptance: one table per class with path:line, catch, static replacement and warm-build count where measured. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-perf
