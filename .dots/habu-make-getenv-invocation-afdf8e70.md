---
title: Make GETENV invocation owned
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T04:23:23.885656+03:00"
---

Public b685c051-xml-zip-bc09ec63 GETENV/ENV=? share ENV-Z/ENV-A/ENV-U and ENV-QA/ENV-QU cursor cells. Two concurrent tasks comparing 100000 TMPDIR lookups with a main-thread baseline return empty/unrelated environment bytes; ZIP then attempts mkstemp under H_TTY=/dev/ttys002 or RM=. Minimal no-ZIP repro retained at /Users/joel/.cache/tmp/bob-zip-default-check.xCX4xS/getenv-task-repro.f. Use invocation-owned stack/locals in bootstrap-phase src/os/env-base.f; preserve captured startup envp/argv base semantics and exact existing absent/empty/prefix matching behavior. Remove obsolete cursor helpers/resets and owned-cell FRESH classifications; update live docs/examples and existing stripped tests so removed names cannot make visibility checks pass vacuously. Dependencies: env-base.f, src/habu/aot-owned-cells.f, tools/native-build-core.f historical example, tools/hb-build-stripped-test.f and docs/native-applications.md. No locks, caching, libc getenv, private consumer engine or ZIP workaround. Inspect existing environment/native/ZIP tests and record real concurrent regression before code. Main core lane owns matching native build/public deployment; qualified concurrent lookup and ZIP consumer plus required native/convergence/recovery checks and independent Astra review precede release. Keep separate coherent feature/dot; package owners retain their own source edits.
