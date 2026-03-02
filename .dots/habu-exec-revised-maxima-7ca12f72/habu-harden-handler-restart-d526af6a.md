---
title: Harden handler/restart/unwind semantics under Maxima load
status: active
priority: 1
issue-type: task
created-at: "\"2026-03-07T19:32:55.801577+01:00\""
---

src/interp/vm.zig:8607-8747 and adjacent handler/restore paths. Root cause: Maxima still stress-tests condition-system corner cases beyond the already-fixed throw/unwind path. Fix: ensure errors degrade to ordinary failures instead of VM aborts and remove remaining handler-stack corruption risks. Why: prerequisite to removing the meval* workaround fully.
