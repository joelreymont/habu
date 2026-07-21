---
title: "Infer: operational metrics + soak"
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T16:45:21.616405+02:00"
blocks:
  - habu-infer-serving-front-63993ff2
---

Plan-of-record M10 operational half: queue length, active sequences, prefill/decode tokens, TTFT, inter-token latency, request latency, KV page use incl. shared-prefix, reserve/headroom, reject/wait reasons, post-warmup page faults, kernel/schedule ids, pack checksum - in a simple scrape format. Plus the eight-hour soak: correctness, leaks (allocator invariants held the whole run), stability. Gates per the doc: malformed requests never corrupt state, slow clients get honest backpressure, cancellation frees everything, restart from the same pack reproducible.
