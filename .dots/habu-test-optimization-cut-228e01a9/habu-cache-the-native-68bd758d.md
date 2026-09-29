---
title: Cache the native fixture writer as a keyed built image
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-29T18:01:21.019630+02:00\""
---

Problem: test/native-fixture-write.f:6-7 recompiles tools/native-emit.f at tier 1 on every fixture write (~20 s of a 20.7 s aot-wid-build run); 14 writes per gate in the AOT lane. Evidence: ~/.cache/tmp/kestrel-gate/test-review/L3-aot-image.md finding 1. Acceptance: the writer is built once per gate as a content-keyed image following test/cold-engine.f (key covers every source that shapes the writer); later writes reuse it; a stale key rebuilds; fixture bytes identical to today's. Files/Ownership: test/native-fixture-write.f, a new keyed-image helper beside it, callers only where the entry changes. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Also show a key change forces a rebuild. Depends: none. Claim: agent=kestrel/worker-max workspace=.jj-ws/habu-cache-the-native-68bd758d
