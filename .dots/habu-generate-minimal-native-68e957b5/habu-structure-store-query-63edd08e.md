---
title: Structure store query results
status: open
priority: 2
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:25:24.499607+02:00"
---

maki/store.f:238-270,316-350,387-420 returns persisted query data through bespoke trailing booleans: STORE-MATCH?/STORE-QUERY/EVID-GET/PROFIT-GET/CALIB-GET expose ptr+length+bool, SCHED-GET returns n+bool with -1 on absence, and MEAS-GET returns two n values+bool with -1/-1 placeholders. Payload fields remain ordinary stack values on the absent path, so callers can consume fabricated pointers, lengths, candidates, or measurements without MATCH. Define a STRUCTURE text-span for owned query suffixes, nominal candidate-id where schedule identity is intended, and a STRUCTURE measurement-row with candidate and median fields. Return option<text-span>, option<candidate-id>, and option<measurement-row>; all callers must exhaustively MATCH before accessing payload fields. Keep file absence as none and propagate actual I/O/parse errors. The internal scan may use one typed found-state payload ENUM instead of STORE-Q-FOUND plus ambient result length. Preserve append-only latest-wins behavior, exact on-disk bytes, missing-file semantics, parsing errors, and sealed writer boundaries. Add checker negatives proving absent payload access and result/field swaps reject; exhaustive none/some/latest/malformed/capacity tests and byte-golden store replays. Measure JIT/DATA/CODELEN and query throughput before/after. Files: maki/store.f and every direct caller/test. Verify store/replay/report/promotion suites, Maki, typed-local diff, type/package/host/filemap/dot lints, and full native gate. Ownership: query-result and measurement representation only; persistence schema/promotion authority remain separate.
