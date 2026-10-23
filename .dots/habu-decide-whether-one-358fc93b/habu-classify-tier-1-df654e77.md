---
title: "Classify tier 1's gains in hot words"
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:18.985901+03:00"
---

For the hot words of the timed workloads, dump tier-0 and tier-1 code (tools/tier-dump.f; llvm-objdump until habu-decode-every-arm64-6753979c lands) and attribute each word's gain: primitive inlining, register-resident stack values, loop counters in registers, compare/branch fusion, loop fold, register allocation and spills, other. Depends: 'Time real workloads at both tiers'. Claim: unassigned.
