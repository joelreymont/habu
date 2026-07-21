---
title: Orin sm_87 goldens for RMSNorm/RoPE kernels
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T22:33:46.398328+02:00"
blocks:
  - habu-provision-jetson-orin-18e0610d
---

Loose end from the RMSNorm/RoPE kernel landing (c49a7331): the Orin sm_87 forward goldens and profile rows for RMSNORM-ROWS/-BWD and ROPE-ROWS/-BWD are OWED - the Orin box was unavailable, so proof was carried on the GB10 only. The debt is recorded in the device-test headers, FILEMAP, and the 4 WAIVER rows in tools/ptx/perf-rows.tsv. When the Orin (zed) is reachable (see habu-provision-jetson-orin-18e0610d / habu-infra-upgrade-orin-3a88aebb): run the two device tests there, record the sm_87 goldens, replace the WAIVER perf rows with measured rows, and clear the OWED notes. Blocked on Orin availability, not on any code.

2026-07-21 addition: the device-LN landing (tools/ptx/layernorm-cg.f + layernorm-device-test.f, stack cb1e4cae) carries the same Orin-owed debt as rmsnorm/rope: sm_87 goldens + measured perf rows replace its two WAIVER rows when zed is reachable.
