---
title: Hermeticize perf executables
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.339799+02:00"
blocks:
  - habu-make-benchmark-bring-617f62fe
---

Problem: perf and validation tooling can drift by cwd, /tmp helpers, or non-repo binaries. Acceptance: tools pin repo-root binaries and reject ambient executable paths for authoritative runs. Files: tools/maxima-hotspots, tools/validate-session, tools/perf-loop, tools/maxima-bench, tools/run, tools/build. Verify: tooling refuses cwd-relative or tmp-based authoritative runs. Blockers: habu-make-benchmark-bring-617f62fe.
