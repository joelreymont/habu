---
title: "Recognise FUNCTION: by resolved identity in verify"
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T02:12:52.149528+01:00"
---

Problem: src/habu/verify-source.f recognises FUNCTION: and ;FUNCTION by spelling (TOP arm, FFI-CLOSER?). A package defining its own FUNCTION: verifies as E-VS-MISSING-SIGNATURE but loads. Predates F2b (txmnqsxn 2026-10-03). Acceptance: FUNCTION:/;FUNCTION are recognised only where the load's lookup binds them to the declarer's words (as FFI-CLAUSE? does via LIBRARY-DEFINER?); a row with a package-local FUNCTION: certifies and loads. Verify: test/ffi-struct-verify.f, tools/check-verify-test.f.
