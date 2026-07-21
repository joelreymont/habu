---
title: Own report instances explicitly
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:03:14.929022+02:00"
---

maki/report.f presents report handles over process-global backing tables, so two apparent reports alias and reset/rebuild can make earlier handles observe new state. Define a package-owned report instance value backed by owned storage with generation-qualified identities for candidates, selections, warnings, profiles, and rendered spans. Construction returns an independent instance; every mutation consumes or threads that instance, and immutable snapshots remain valid across later reports. Remove singleton storage and reset words rather than adding more reset protocol. Integrate the existing candidate-id and report-column typing dots as field/domain owners instead of duplicating them. Add two independent and interleaved reports, nested rendering, stale-generation rejection, failed mutation preserving the old report, capacity/canary cases, snapshot stability, and exact output goldens. Measure and bound per-instance storage; no ambient fallback instance or compatibility global. Files: maki/report.f, focused tests and direct callers. Verify report/store/schedule/promotion suites, Maki, typed-local/package/host/filemap/dot lints, and full native gate.
