---
title: Build a --repl program that loads power.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T22:15:11.287793+02:00"
---

Problem: hb-build --repl of a program whose source is 'require src/lib/power.f' and ': MAIN ( -- ) ;' fails rc 74 with 'power.f:25: linear: '>AWAKE': unknown type 'POWER:awake'' E-CAST-FAM (installed hb on master dd2aa42a plus the aio host-load fix). src/lib/power.f:20 declares 'DEFLINEAR POWER:awake' inside package POWER, and line 24 'LINEAR: >AWAKE ( n -- POWER:awake )' cannot find it during a build, while loading power.f with hb --load (src/lib/power-test.f) passes. Cause unknown: a type a file declares, named qualified in the same file, is not found during a build. Acceptance: that program builds rc 0 and its product runs; warm build == cold. Files: named by the root cause. Verify: env TMPDIR=$PWD/.out/t ~/.local/bin/hb --load tools/hb-build.f -- --repl <ws>/.out/power-repl.f -o <ws>/.out/power-repl </dev/null, then the warm rebuild and cmp. Depends: none. Ownership: unassigned. Claim: unassigned.
