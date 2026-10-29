---
title: Recovery engine reads NATIVE-HOST-CELLS as stale
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T04:04:24.880740+02:00"
---

On master b115ac7fc1, tools/bootstrap.sh with GFORTH=$HOME/.local/bin/gforth (0.7.9; /opt/homebrew 0.7.3 does not work) installs a recovery engine, but its preload native-build fails at tools/native-layout.f:34 'at , unknown cell ... actual: stale<n>'. Reduction: in the recovery engine 'NATIVE-HOST-CELLS:SELECT .' (also FACTS, DIVISION-PRIM) refuses stale<n>, while 'HOOK-CELL .' and 'NATIVE-OBS-CELLS:PUBLISHED .' work; the public engine prints 11552. native-host-cells.f is first required by src/habu/habu2.f and appended as a module by tools/build-fixpoint.f:746. Guilty commit not found: bisect bootstraps (4-6 min each). Workspace: none yet.
