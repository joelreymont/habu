---
title: gate-runner-support standalone --load fails (missing requires)
status: open
priority: 3
issue-type: task
created-at: "2026-07-05T00:06:00.000000+02:00"
---

Found 2026-07-05 while building an isolated phase-17 harness. The resident
runner support files do not load standalone via `bin/hb --load`, because they
rely on the full ordered resident load rather than requiring their own deps:
`bin/hb --load tools/public-signatures-core.f` fails E-UNDEFINED: COPY-UPPER,
and loading test/gate-runner-support.f's require list standalone dies rc 77
(E-LINT-TOKEN-CAP) from tools/lint/token.f. These files work only because the
full test/run.f resident image loads everything in the right order and forks
phases copy-on-write. Fix: make each support/tool entry require the exact deps
its own top-level path uses (per the "entries require their own dependencies"
lesson), so `bin/hb --load <support-file>` is loadable in isolation and a
per-phase harness (e.g. run-worker-<family>.f driven directly) works without
the whole DAG. This is the same theme as habu-gate-stdlib-standalone-098d7f57
but for the gate-runner support family, not the gate-stdlib entry.
