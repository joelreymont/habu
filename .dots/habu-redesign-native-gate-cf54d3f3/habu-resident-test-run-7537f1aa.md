---
title: Resident test/run phase dispatcher
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T21:26:57.674953+02:00"
blocks:
  - habu-split-gate-runner-59bb0aaf
---

Problem: test/run.f still treats hb-gate-warm as the default execution engine once ready, so a cold suite pays broad runner image build time and a hot suite still launches a runner process for each semantic phase. Fix: after gate runner dispatch is importable, load/require runner support once in the resident suite process, execute host-source semantic phases through direct checked dispatch words, and keep subprocesses only for true CLI/candidate/artifact boundaries. Acceptance: cold Mac suite no longer builds the broad hb-gate-warm image for resident-safe phases; hot Mac remains <=30s; test report still prints named tests/timings; stats distinguish resident vs process boundaries; zed proof remains explicit if unavailable locally. Files: test/run.f, test/gate-runner-lib.f, test/gate-stats.f/docs as needed.
