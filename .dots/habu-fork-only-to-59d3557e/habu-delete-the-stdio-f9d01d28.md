---
title: Delete the stdio spawn primitives
status: open
priority: 1
issue-type: task
created-at: "2026-10-28T20:27:02.267724+02:00"
blocks:
  - habu-convert-spawn-callers-791e8a01
---

Fork-to-exec lane 1d: delete spawn-io, spawn-argv-io, spawn-argv-env-io, spawn-argv-env-cwd-io, SPAWN-ACTION-CAP and their engine-suite and x86-64 kernel cases. run-rc moves onto spawn-fds with the map 0 1 2 (carl: a child's fd table is exactly its map, so run-rc no longer leaks inherited fds). Full gate, convergence, x86-64 on joel@pulsar-ua-106. CLOSE-EXCEPT2, MAXFD and IO-CLOSE-HIGH serve fork children only and go in lanes 4-6. Claim: carl.
