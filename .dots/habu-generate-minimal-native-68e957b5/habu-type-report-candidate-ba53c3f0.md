---
title: Type report candidate selection
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:25:39.821183+02:00"
---

maki/report.f:566-584 stores schedule selection as raw F-SELECT where -1 means none and every other n is intended to index L-CAND. SELECT!/SELECT@ therefore expose an optional semantic relation as an unchecked integer; candidate counts, warning indexes, other report indexes, and arbitrary n are interchangeable, and stale ids remain representable after reset/rebuild. Introduce a report-owned nominal candidate-id returned by candidate insertion/lookup, represent selection as option<candidate-id> in typed storage, and make SELECT!/SELECT@ accept/return that type. Validate/refine raw indexes only at the bounded iteration/parse boundary; rendering and persistence exhaustively MATCH none/some. If candidate identity is generation-sensitive, bind it to the report generation so ids from a reset or another report cannot select. Preserve candidate order, no-selection behavior, report bytes, schedule persistence, and errors. Add checker negatives for raw n, warning/profile/foreign candidate ids and cross-report/generation selections; runtime tests cover none, first/last, stale/reset, out-of-range refinement, and render/store round trips. Measure JIT/DATA/CODELEN and report rendering throughput before/after. Files: maki/report.f, store adapters and focused tests. Verify report/store/schedule/promotion suites, Maki, typed-local diff, type/package/host/filemap/dot lints, and full native gate. Ownership: candidate identity and optional selection only; report declaration syntax and other rows remain separate.
