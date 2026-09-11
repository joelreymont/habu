---
title: Type store replay lifecycle
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:49:01.885775+02:00"
---

Current master defect: maki/store-replay.f:72-110 documents replay-state as cold | ready | failed(error) but implements raw RPS-* integers plus independent SRP-STATE and SRP-ERR cells at 85-89. Cold or ready with stale nonzero error, failed with zero, and arbitrary states are representable. REPLAY-RESET at 95 and the failure path at 108-110 publish the fields separately; REPLAY-ERROR at 94 exposes a code even when the state is not failed. Replace both cells with payload ENUM replay-state = cold | ready | failed(code) stored in TYPED-VARIABLE. Make reset, ensure, retry, readiness, and error queries use exhaustive MATCH; expose an error only as option<code> or through the failed payload. Preserve one-attempt fail-closed recovery, original throw identity, durable bytes, and no-partial-row guarantee. Prove compile-negative raw state/error/foreign-enum writes and non-failed payload access; full initial/reset/success/failure/retry transition table; injected failures before and after validated scan leave no partial rows and retain exact original code; no observable half-transition; store/replay byte parity. Measure JIT/DATA/CODELEN and ensure-path latency before and after. Coordinate habu-structure-store-query-63edd08e, which owns query result shapes rather than this recovery lifecycle.
