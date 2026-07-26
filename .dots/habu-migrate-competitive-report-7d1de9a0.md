---
title: Migrate competitive report records and sums
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T12:16:33.132069+02:00"
---

Wave D1 of the unified-type migration program (.blackboard/migration-plan-20260726.md). maki/competitive-report.f: :91 gbps 0 and :95 gflops 0 (at n / na BENCH:absence) become full-mode payload ENUMs, FIELD names from source; :101 comparison-gbps 0 and :112 comparison-gflops 0 are NINE-field PRODUCTs whose fields are typed by the two sums in the SAME file - the ordering constraint: the sums must be migrated and loadable before the records that reference them, all in one commit; records become STRUCTURE per 6ef124d0c64e with byte-identical FIELD lines. Consumers (tools/eval-triton.f, maki/competitive-store.f) untouched, spelling-preserved, their suites run explicitly. A1 pattern for the sums; for each nine-field record, the C13 discipline: name-to-slot registry pins for all nine fields (value round-trips are proven blind to same-type swaps) plus a distinct-value round-trip and one adjacent-swap mutation kill per record. STOP conditions per program plan; nine-field names must clear the reserved gate and the 32-byte cliff. Acceptance: competitive suites, eval-triton suite, maki/test.f green; both diff lints; census verify identical (all full-form). Claim: agent=mig-d1 workspace=.jj-ws/habu-mig-d1

Closed 2026-07-26: landed as ac037264b8ba. Measured slot layout (two-cell field shifts base to slot 9); subject-vs-baseline exchange provably caught only by registry pins; cliff control with real unresolvable control.
