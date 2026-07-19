---
title: Structure transcript parser state
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:24:00.913414+02:00"
---

maki/eval/transcript.f:64-106,125-160,190-271 models transcript version, token source, recorded verdict, current task, and sample lifecycle with raw n cells, booleans, zero, and -1 sentinels. TS-FIND returns -1, TS-HDR-VER returns 0 for not-a-header, TS-REC accepts any positive value as a recorded result in TS-CLOSE-SAMPLE, and TS-HDR?/TS-OPEN?/TS-CAND#/TS-REC/TS-PEND? admit contradictory parser states. Each task tally is seven positional numeric slots addressed by raw constants. Define a nominal task-id, closed ENUMs for transcript-version, token-mode, and recorded-verdict, option<task-id> for lookup/current selection, and payload ENUM parser/sample state so directive order and pending-token state are explicit. Define STRUCTURE task-tally with named sample/green/repaired/round/token/recorded/estimate fields and store typed rows instead of TS-SLOTS arithmetic. Parse boundaries construct typed values; all control dispatch uses exhaustive MATCH. Preserve accepted transcript grammar, ordering errors, v1/v1.1 behavior, exact counts, rendered reports, proxy/model-token exclusivity, and capacities. Add checker negatives for task/version/mode/verdict/tally-field swaps, exhaustive state-transition/directive tests, malformed version/verdict/token cases, and byte-golden real transcripts. Measure JIT/DATA/CODELEN, table bytes, and feed throughput before/after. Files: maki/eval/transcript.f and focused consumers/tests. Verify transcript/repair/eval suites, Maki, typed-local diff, type/package/host/filemap/dot lints, and full native gate. Ownership: parser state, semantic domains, lookup result, and tally representation only.
