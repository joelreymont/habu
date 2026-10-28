---
title: Delete the test-only NHOST FACTS chain
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T22:36:19.984438+02:00"
---

L3-C4. Joel 2026-10-28 ruling: delete host.f ASSOC table (ASSOC-N/ENTRY/LEN/IMPL/LIVE), MATCH-FACTS, PRIOR-FACTS, AS-FACTS, TARGET-LEN?, NHOST:ASSOCIATE, the engine header cell NATIVE-HOST-CELLS:FACTS ($2D30; src/habu/native-host-cells.f, data-claims.f:192, snap-lib.f:356, tools/native-layout.f:35) and test/compiler/native-host-bundle-check.f 176-179 uses; NHOST:ID-OF too if then unused. Evidence (C3 review): FACTS is read only by TARGET-LEN?, called only by ASSOCIATE, called only by that test; ASSOC-IMPL write-only. Hard cutover, no version. Base: C3 final (yltmqoko). Header layout change: check whether landing needs a staged host (docs/bootstrap.md).
