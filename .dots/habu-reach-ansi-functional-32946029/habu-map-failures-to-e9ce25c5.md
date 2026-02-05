---
title: Map failures to subsystems
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-05T22:32:05.470626+01:00\""
closed-at: "2026-02-05T22:43:55.830891+01:00"
close-reason: Added docs/ansi-failure-map.md with one-to-one failure bucket mapping.
blocks:
  - habu-record-functional-parity-34a77dda
---

Context: /Users/joel/Work/habu/docs/ansi-parity-baseline.json:new, /Users/joel/Work/habu/docs/ansi-failure-map.md:new; cause: failures are not grouped for execution; fix: classify each failing test into subsystem buckets with owner files and priority; deps: habu-record-functional-parity-34a77dda; verification: every failing test id appears in exactly one subsystem bucket.
