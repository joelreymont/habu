---
title: Capture served samples
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.846769+02:00"
blocks:
  - habu-infer-serve-one-c0c151d2
---

Why: performance data must come from the exact loopback product path before documentation reduction. Interface: package SERVE-MEASURE owns tools/serve-measure.f and RUN; the checked command starts one fresh explicit server per sample and opens one or four nonblocking clients through SOCK-OS:CONNECT. connected(conn) enters the send state. connecting(conn) enters a bounded connect state that alternates SERVE:RUN-ONCE with writable polling under the same fixed workload deadline; writable readiness calls FINISH-CONNECT, connecting repeats the state, and only connected may enter send. CONNECT or FINISH-CONNECT refusal and deadline expiry attempt CLOSE once for every client and reject the sample, including any consumed close error. After all clients connect, RUN alternates each bounded client send/read transition with SERVE:RUN-ONCE in one thread until every complete response arrives or the deadline fails. It validates exact response text, finish reason, and usage, then emits raw latency, generated-token rate, and immutable owner FOOTPRINT rows. Owner: raw served-sample capture only. Production red: no command measures the shipped socket path. Acceptance: immediate and pending connect, spurious writable wakeup, SO_ERROR refusal, connect timeout, GPT-2 four-client, and Qwen one-client workloads follow exact ownership; no byte is sent before connected; any timeout or response mismatch rejects the sample; owner bytes and staging high-water come only from model, DEVRT, KV, SCHED, and SERVE queries. Forbidden: report writing, general client harness, percentile framework, threshold, benchmark schema, global free-memory inference, adapter, database, dashboard, version, or optimization. Smallest owning check: bin/hb --load tools/serve-measure.f on DGX Spark.
