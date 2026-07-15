---
title: "Kernel perf: ratchet waiver on touch"
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T07:29:54.769623+02:00"
blocks:
  - habu-lint-diff-share-486c2d86
---

Full context: tools/ptx/perf-rows.tsv WAIVER rows can satisfy kernel-perf-lint indefinitely and the 12-field registry carries no checked emitter identity; wall-clock N-day expiry would make builds nondeterministic. Fix: extend the checked PERF registry schema with canonical emitter/kernel ownership for waivers and expire a waiver deterministically on the next diff touch of that emitter/kernel unless the same change adds a replacement measurement or a newly versioned waiver reason. Reject cross-kernel waivers, duplicate live waivers, unknown emitters, stale rows, and reordered/forged identities. Acceptance: unchanged historical waiver remains valid; touching its emitter without replacement fails; another emitter's waiver cannot satisfy it; a same-change replacement row passes; committed TSV, docs, parser, and renderer stay canonical. Files: tools/ptx/perf-registry.f, tools/ptx/perf-registry-test.f, tools/ptx/perf-rows.tsv, tools/kernel-perf-lint-core.f, tools/kernel-perf-lint-test.f, docs/kernel-principles.md. Verify: registry mutation matrix, kernel-perf focused suite, regression scan, host/filemap/trust/dot gates.
