---
title: Preserve fresh region effect templates
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T11:29:34.868532+03:00"
closed-at: "2026-10-20T18:19:58.233173+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

A real warm-replayed d2 engine fails existing test/c2-owner-producer-refusals.f assert12: two C2-MEM:ALLOC calls incorrectly satisfy one fresh-region identity. Cold engine rejects correctly. The sole active saved ALLOC effect contains EN-ATOM region-a EN.C=-1 cold versus +1 warm; E-INST-FROM only mints negative templates, so warm calls share concrete id1. Locate the responsible publication/replay/serialization boundary and preserve templates and per-call freshness; do not zero arbitrary effect atoms or weaken ownership. Existing owner E2E must fail before/pass after, alongside exact cold/warm bytes and required native/convergence.
