---
title: Package debugger watchpoints
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:28:30.270792+02:00"
---

src/habu/debug-watch.f:7-71 exposes 18 globals, 17 with BPW-* prefixes, for the REPL stepper/debugger watchpoint table. stdin.f AOT-compiles and seeds this module into every installed bin/hb; only its commands are invoked on demand, so it is reachable product code rather than an on-demand module or dead payload. Table storage, slots, formatting, validation, and installation mechanics need not remain global. Put the module in package WATCH, export only add/remove/list-or-dump/clear if retained plus the qualified boot install entry proven by callers, keep all table/state/helpers private, and update stepper/debugger/stdin callers directly without aliases. Preserve REPL commands, watch matching, capacity/error behavior, rendered disassembly/state output, AOT records/relocations, snapshot restoration, and dormant-startup behavior. Add old-global/private rejects, qualified public positives, capacity/removal/reuse tests, and a REPL invocation proving the feature remains callable but unexecuted at startup. Measure persisted name bytes, loaded JIT/DATA/CODELEN and startup/debug command latency before/after. Verify debugger/stepper/REPL/AOT/snapshot/fixpoint tests, package/host/filemap/dot lints, and full native gate. Parent: habu-pkg-native-build-f598557c.
