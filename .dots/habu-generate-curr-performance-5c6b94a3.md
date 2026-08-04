---
title: Generate current performance reports from evidence
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:03:51.665002+02:00"
---

Live performance docs and paper tables mix retracted, stale, unregistered, and current measurements, including contradictory GEMM claims, obsolete Orin 1.60x language, rounds absent from the registry, and stale binary/suite counts. Make the checked evidence store the only source for current performance claims. Each accepted row binds artifact, target, toolchain, numeric policy, workload, command, referee, clocks/environment, date, and validity state; retracted rows remain immutable historical evidence but are excluded from current projections. Generate current reports, best tables, paper numbers, and reproduction commands from the store, with explicit historical/archive links. A live document may not hand-copy a performance number. Import every still-valid row through the schema, mark unverifiable scratch-only rows historical, and reconcile round identities. Add stale/retracted/unregistered/mismatched-policy mutations, source-doc literal detection, report regeneration identity, and paper-table consistency tests. Reuse the checked BENCH and performance-registry schemas rather than create another measurement format. Files: evidence renderers, current performance docs/paper generated regions, archives, focused gates. Verify registry/report/competitive evidence suites, docs path/value lints, host/dot lints, and full native gate.
