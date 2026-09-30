---
title: Let check.f see names a library parsing definer makes
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T17:10:53.124813+02:00\""
---

Problem: a name made by a library definer that renders source and hands it to INCLUDE-EVALUATE at load (lib/ffi-abi.f FUNCTION:, lib/process-command.f COMMAND, lib/task.f +USER) does not exist for the source pre-pass, so tools/check.f refuses a later mention with E-UNDEFINED while the real load path accepts it. Measured 2026-09-30 on the r4-check engine (change pzrymmlt): `require lib/ffi-abi.f  PROCESS-SYMBOLS  FUNCTION: G getpid ( -- i32 ) ;FUNCTION  : H ( -- n ) G ;` loads rc 0 and checks rc 70 (E-UNDEFINED, token G); `bin/hb --load tools/check.f -- test/gate-images.f` exits 70 on SELF-PATH (lib/engine-id.f:70, declared by FUNCTION: at :48). Acceptance: the pre-pass learns such products by a rule that names no library definer in the engine (src/habu/verify-source.f "WHY LEARNED AND NOT LISTED"); check.f accepts the reduced input and test/gate-images.f and still refuses a name nothing defines; cases run through the real check.f load path (tools/check-test-lib.f), written before the code. Files: src/habu/verify-source.f, src/core/checker.f, the definers, tools/check-test-lib.f, docs/forth.md. Verify: `bin/hb --load tools/check.f -- test/gate-images.f` rc 0; tools/check-test.f rc 0. Depends: habu-make-check-f-ab22f852. Ownership: those files. Claim: agent=kestrel workspace=.jj-ws/r4-check.
