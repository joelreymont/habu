---
title: Test a header store before later program files
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T19:26:17.953680+01:00"
---

Problem: linker.f's tracking of a header-data store made by a file that later files follow (WALK-STORERS header branch, HSTORE+) lost its tests (file-objects HEADS, HELDS, SAMES) once data-base left build files; a program file still reaches it by calling APP-ENTRY:XT! (src/habu/layout.f:1204-1210), which app-end.f stores only under --repl (tools/hb-build-core.f:264). Acceptance: a program build (tools/hb-build.f) whose non-last file calls APP-ENTRY:XT!, warm-rebuilt after a later file changes, runs that entry, cold and warm alike. Depends: the DT10 lane (nxusyqpo) lands. Claim: unassigned.
