---
title: Resolve an IR arena handle once per read
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.253483+03:00\""
---

Problem: every IR-ARENA read (READ/FREAD/SIZE/USED) runs FROZEN-SLOT/LIVE-SLOT, RESOLVE, FIND-A, AHANDLE@, AOWNER@, IR-CTX:SERIAL-LIVE?, FIND-SLOT, HANDLE@, ASTATE@, READ-SLOT, ORDINAL-CHECK, ACOUNT@, CELL-AT, ADATA-FIELD, CDIGEST:SLOT@, NATIVE-SLOT?: 16 calls, 52 ns per resolved read, 1.774 G resolutions and 1.233 G cell reads per Tender load, 52% of 176 s. Acceptance: each public reader resolves once and performs the consumed-handle, dead-owner (retire on touch), state, ordinal and bound checks inline in that order with the same error on the same input; SERIAL-LIVE? is a direct compare with the zero guard; CELL-AT reads the cell directly when the host is native-cell (probed once at load, alignment proved from SCRATCH-TAKE); no new public pointer or raw converter; negatives pin the check order; controlled pair on 41df9051. Files: src/compiler/ir/arena.f, src/compiler/ir/context.f, test/compiler/ir-arena.f. Verify: ir-arena, ir-context, ir-storage-schema, ir-build, ir-op, ir-fun, ir-type, ir-symbol, ir-schema, ir-attr, ir-source, ir-verify suites; forced-tier Tender pair. Depends: none. Ownership: rowan, workspace .jj-ws/rowan-arena (in flight). Claim: agent=rowan workspace=.jj-ws/rowan-arena
