---
title: "Infer M0: pinned baseline results"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.788086+02:00"
blocks:
  - habu-infer-m0-vllm-cf32a8d5
  - habu-infer-m0-lightweight-0d25ed99
  - habu-infer-m0-unified-12b25bac
  - habu-infer-m0-benchmark-c50501b7
  - habu-infer-m0-benchmark-67ece165
---

Why this exists:
M0 is incomplete until the pinned checkpoint and prompt suite have reproducible vLLM and lightweight-engine results on this DGX Spark.

Required result:
run the validated adapters over the core matrix on an idle machine and commit canonical result records with raw-log digests.

Done when:
every required cell has the declared repetitions and metrics, failures remain visible, rerunning the reducer is byte-identical, and the report makes no Habu performance claim.

Expected touch points: benchmark result records under data/infer-bench/, concise result note under docs/.
Smallest check: schema validator and reducer over every committed record.
Prerequisites: vLLM adapter, lightweight adapter, unified-memory counters, workload matrix, and benchmark reducer.
Owned result: first baseline data and report only.
Claim: unassigned.
