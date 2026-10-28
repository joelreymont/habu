---
title: Delete the test-only NHOST FACTS chain
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T22:36:19.984438+02:00"
---

L3-C4. Joel 2026-10-28 ruling: delete host.f ASSOC table (ASSOC-N/ENTRY/LEN/IMPL/LIVE), MATCH-FACTS, PRIOR-FACTS, AS-FACTS, TARGET-LEN?, NHOST:ASSOCIATE, the engine header cell NATIVE-HOST-CELLS:FACTS ($2D30; src/habu/native-host-cells.f, data-claims.f:192, snap-lib.f:356, tools/native-layout.f:35) and test/compiler/native-host-bundle-check.f 176-179 uses; NHOST:ID-OF too if then unused. Evidence (C3 review): FACTS is read only by TARGET-LEN?, called only by ASSOCIATE, called only by that test; ASSOC-IMPL write-only. Hard cutover, no version. Base: C3 final (yltmqoko). Header layout change: check whether landing needs a staged host (docs/bootstrap.md).

Also (Joel 2026-10-28 ruling): shrink the file-object address row (section 3) from 40 to 24 bytes, site/kind/ref, dropping the two always-zero cells at 16 and 32 (tools/native-unit-object.f DEF-ADDR+/DEF-PREP-ADDR/DEF-ADDR-BYTES, native-file-reference.f ADDR-BYTES and R-SITES offset 24); width and ref offset as NUNIT-FILE constants beside CALL-ROW-BYTES/CALL-REF-OFF (added in C3). No version bump.

Joel 2026-10-28 ruling: also delete NHOST IMPL-CONTRACT (write-only) and PREPARE's contract argument. Lead additions (dead once ID-OF went): IMPL-LIVE, IMPL-ENTRY, SEEDED-N, INVALIDATE's row retirement, PUBLISH-TAKEN's liveness loop. Staged host not needed: tools/native-build-core.f EMIT-TEMP drops an older host's $2D30 xt row, as for the retired $3838 cells. Work: onrqnwsp in .jj-ws.noindex/jerry-c4; gen-1 (built by C3 candidate) == gen-2 byte for byte; full suite and code review running.
