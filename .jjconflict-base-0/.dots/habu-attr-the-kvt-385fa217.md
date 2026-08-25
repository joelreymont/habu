---
title: Attribute the KVT-NO-ALLOC fork SIGBUS
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:22:30.786612+02:00"
---

maki/infer/kv-cache-test.f KVT-NO-ALLOC's fork child takes a native SIGBUS under mmap exhaustion — observed during the kv-cache attribution (lane kv-attr, 2026-08-05), independent of the missing CUDA driver, unattributed. Debug with the debugger per docs/debugging.md on a device host (the suite is device-required); find whether the SIGBUS is the intended failure-injection outcome or a real crash in the allocation path under exhaustion.
