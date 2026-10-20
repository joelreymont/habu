---
title: Lift the remaining 64-byte native name caps
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T11:04:22.787369+02:00"
closed-at: "2026-10-14T19:38:01.469404+03:00"
close-reason: "Implemented in d3f3a6f8: native refusal tokens use the definition capture bound, unit names borrow the source span or refuse the format limit by name, and preseed names refuse their maker limit; real-entry cases and convergence passed."
---

Found by lane 3d1ce798: src/compiler/native/elaborate.f:62 RF-CAP 128 drops the ' at <token>' part of a tier-1 refusal when the token is over 128 bytes (probe rf1-200, same before and after); tools/native-unit-compile.f:13 and tools/native-unit-file.f:21 cap unit package names at 64; tools/hb-build-lib.f:72 caps a test-only preseed entry's name at 64. Acceptance: each either holds any name the engine admits (7999 bytes) or refuses a longer one by name, tested through its tool's real entry.
