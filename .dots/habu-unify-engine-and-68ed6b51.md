---
title: Unify engine and application build sessions
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T17:07:10.163659+03:00"
---

Existing tools/native-build.f cold-loads unchanged engine source while tools/hb-build.f uses FILE cache; docs/compilation.md requires one build program. Implement the existing approved Astra design: one core with engine, REPL and stripped prepare/load/finish continuations, one retained support selection, existing FILE family extended for portable bound references/dependencies/CWORK/checker owners, explicit source-owner/loader rebind at checker handover. Bootstrap recovery remains separate. No plugin registry, extra parser, byte-value pointer guesses, end-build sealing or per-engine cache clone. Acceptance: real engine/REPL/stripped commands preserve output/refusal/root/CLI semantics, early/middle/late edit reuse and complete invocation measurements; real E2Es before implementation, full native and uncached convergence, Astra review. Parent active59200861 remains actual unfinished engine incremental objective.
