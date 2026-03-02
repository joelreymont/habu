---
title: Make probe-file and truename truthful
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.224250+02:00"
blocks:
  - habu-unify-stream-and-a262987a
---

Problem: probe-file and truename collapse into echoed input or ambient cwd behavior. Acceptance: canonical CL failure contracts and truthful path results. Files: io/path primitives, lib/stdlib.habu pathname helpers. Verify: pathname regressions for existing, missing, and trusted-root-confined paths. Blockers: habu-unify-stream-and-a262987a.
