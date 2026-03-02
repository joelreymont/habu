---
title: Cut over to one authoritative build path
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.357838+02:00"
blocks:
  - habu-make-summaries-require-200d3c73
---

Problem: multiple build wrappers weaken provenance and allow stale binaries into authoritative reports. Acceptance: one canonical supported build entrypoint remains for authoritative tooling and legacy wrappers are removed or demoted. Files: tools/build, tools/build.lisp, helper wrappers. Verify: authoritative helpers prove successful build provenance before publish. Blockers: habu-make-summaries-require-200d3c73.
