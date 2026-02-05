---
title: Dispatch condition throws via handler-bind
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-05T23:30:55.654425+01:00\""
closed-at: "2026-02-05T23:32:31.255377+01:00"
close-reason: Routed %condition% throws to matching handler-bind handler and made find-restart return restart object symbols; updated compute-restarts repro expectations.
---

Context: /Users/joel/Work/habu/src/interp/vm.zig:3208,3331,5459; cause: %condition% throws bypass handler stack and find-restart returns boolean, preventing handler-driven restart control flow; fix: in doThrow route %condition% to most-recent matching handler, invoke handler in current VM flow, and make find-restart return restart object (symbol proxy) not t; deps: habu-add-cond-restart-0e4fd31b; verification: compute-restarts/find-restart repros updated to conforming expectations pass.
