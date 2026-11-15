---
title: Unblock x86 inline checked stores
status: closed
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.371288+03:00"
closed-at: "2026-11-04T11:48:48.016039+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. Tier-1 inline stores via shadow tag and helper-registry merge (aot-native-merge); on intel only, idle since 2026-10-23."
---

Completed reviewed lyoswrvw5604b5be, pushed intel-pstore. Inline cell/byte stores preserve values33,2,33. Exact helper scope uses unused shadow tag00; validator/merge preserve scope, x86 resolver selects private helper registry, Wasm refuses unsupported scope. Two review collision findings fixed; focused follow-up no findings. Native build, gen5/gen4 byte identity, store/native-merge/Wasm tests and linkage/collision probes pass. Existing assertions preserved; additions in tier1-inline-prims,aot-native-merge,x86-64-link-records report store values, helper merge/import and collisions. OPEN Mac shared-format/cross-link checks, full ARM x86-64-link-records, and integrated native full suite. Integration pending.
