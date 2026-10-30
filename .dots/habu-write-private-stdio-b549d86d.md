---
title: Write private stdio descriptors through FD
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T13:00:24.336869+02:00"
---

Problem: two files still name the standard descriptors with private integer constants after FD:STDIN/STDOUT/STDERR landed (carl, FD review 2026-10-29): src/lib/genio-core.f:85-86 (STDOUT-FD, STDIN-FD; genio-core deliberately does not require src/lib/fd.f, so the FD lane reverted that require) and tools/check-verify-child-lib.f:33-34 (OUT-FD, ERR-FD, passed raw to WRITE at :189-226). Acceptance: both use FD:STDIN/FD:STDOUT/FD:STDERR with FD>N at the raw write boundary, or genio-core states why it cannot require fd.f and the constant stays with that reason; no private stdio constant remains elsewhere (rg '[0-2] constant .*(STD|OUT|ERR|IN)-?FD'). Files: src/lib/genio-core.f, tools/check-verify-child-lib.f. Verify: bin/hb --load test/run.f rc 0. Lane: tim-stdio lytpxtzm 6d2cce6b "Write private stdio descriptors through FD" (on rnoytuym, Opus ACCEPT), taken by carl to land; genio-core.f requiring src/lib/fd.f adds a builder file, which turns ROLL-HIT-CASE red on master (habu-check-a-def-3f58f036). Depends: the FD lane (pxpmrrvm) landed; habu-check-a-def-3f58f036. Ownership: carl. Claim: carl (tim-stdio stays in place until it lands).
