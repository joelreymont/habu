---
title: "Leave the profiled program's words unnamed in perf-map"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T19:08:36.732641+01:00"
---

Problem: tools/perf-map-core.f resolves an address through its own process's index, where its own words sit at the addresses the profiled program compiled its words to (0x2a01080 came out as PERF-MAP:IN-CAP), against what its LINE$ comment says. Acceptance: an address of a word the profiled program compiled stays hex while engine words keep their names, shown by tools/perf-map-test.f. Depends: none.
