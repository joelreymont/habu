---
title: Retire the refuted sampling instrument
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-15T13:47:56.186107+02:00\""
---

Claim: agent=lint-sweep workspace=.jj-ws/habu-lint-sweep

Simplification found by the audit sweep: tools/perf/protcost.py is PROVEN to under-report syscall time ~5x (the lprot landing measured 12.5ms where trace-replay and wall/sys agree on 67-80ms) - a wrong instrument invites the next misattribution. The trace-replay method (write the syscall sequence, replay against a fresh mapping) superseded it and is recorded in LESSONS. Delete the tool, or rewrite it on the replay method if a python entry point is still wanted; either way the docs/debugging.md row updates. Files: tools/perf/protcost.py, docs/debugging.md. Depends: none.
