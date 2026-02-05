---
title: Add conformance CI gates
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-05T22:32:05.524160+01:00\""
closed-at: "2026-02-05T23:46:55.201931+01:00"
close-reason: Added PR smoke + nightly ANSI workflow with baseline regression checks and artifact upload.
blocks:
  - habu-map-failures-to-e9ce25c5
---

Context: /Users/joel/Work/habu/.github/workflows:new, /Users/joel/Work/habu/tools/ansi/run.sh:new; cause: no regression gate for functional parity; fix: add smoke gate on PR + full nightly conformance job with artifact upload; deps: habu-map-failures-to-e9ce25c5; verification: workflow runs and publishes conformance JSON artifacts.
