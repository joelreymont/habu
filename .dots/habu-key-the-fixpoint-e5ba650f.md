---
title: Key the fixpoint stamp on the chain closure
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T18:56:46.255918+02:00\""
---

Stage C: the fixpoint stamp hashes bin/hb + emitted stage sources; a chain seeded as prefix source read from the checkout changes NO stamp input, so a chain edit false-skips the refresh (fixpoint: cached, exit 0). Fold EC:BUILD over migrate.f into the stamp key via CONTENT-KEY:FILE+ exactly as tools/hb-build-lib.f:793-800 HBB-CLOSURE-CK+ already does. Acceptance: edit one byte of src/compiler/ir/op.f, re-run refresh, require a FULL rebuild - and this test must be seen to FAIL before the change. Files: the fixpoint refresh tool. Depends: none (must land with or before Stage B).

Claim: agent=keyfix workspace=.jj-ws/habu-key-fixpoint
