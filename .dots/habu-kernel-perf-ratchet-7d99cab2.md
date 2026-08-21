---
title: "Kernel perf: ratchet waiver on touch"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-15T07:29:54.769623+02:00"
closed-at: "2026-07-19T22:50:13.610540+02:00"
close-reason: "Landed acc7def0-era: waiver-specific 14-field row form (zero committed rows rewritten), PERF:EMITTER? centralized predicate, diff-content-driven expiry via the shared jj-diff parser - touch without replacement fails E-PERF-ROW-MISSING, cross-kernel cannot satisfy, same-change replacement (measurement or newly-versioned waiver) passes, stale/dup/unknown/forged all reject. Full acceptance matrix both-directional; perf-rows.tsv untouched"
---

Full context: tools/ptx/perf-rows.tsv WAIVER rows can satisfy kernel-perf-lint indefinitely and the 12-field registry carries no checked emitter identity; wall-clock N-day expiry would make builds nondeterministic. Fix: extend the checked PERF registry schema with canonical emitter/kernel ownership for waivers and expire a waiver deterministically on the next diff touch of that emitter/kernel unless the same change adds a replacement measurement or a newly versioned waiver reason. Reject cross-kernel waivers, duplicate live waivers, unknown emitters, stale rows, and reordered/forged identities. Acceptance: unchanged historical waiver remains valid; touching its emitter without replacement fails; another emitter's waiver cannot satisfy it; a same-change replacement row passes; committed TSV, docs, parser, and renderer stay canonical. Files: tools/ptx/perf-registry.f, tools/ptx/perf-registry-test.f, tools/ptx/perf-rows.tsv, tools/kernel-perf-lint-core.f, tools/kernel-perf-lint-test.f, docs/kernel-principles.md. Verify: registry mutation matrix, kernel-perf focused suite, regression scan, host/filemap/trust/dot gates.

Claim: agent=perfratchet workspace=.jj-ws/perfratchet machine=spark
