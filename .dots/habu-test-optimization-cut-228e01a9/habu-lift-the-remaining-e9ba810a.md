---
title: Lift the remaining 64-byte native name caps
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T11:04:22.787369+02:00"
---

Found by lane 3d1ce798: src/compiler/native/elaborate.f:62 RF-CAP 128 drops the ' at <token>' part of a tier-1 refusal when the token is over 128 bytes (probe rf1-200, same before and after); tools/native-unit-compile.f:13 and tools/native-unit-file.f:21 cap unit package names at 64; tools/hb-build-lib.f:72 caps a test-only preseed entry's name at 64. Acceptance: each either holds any name the engine admits (7999 bytes) or refuses a longer one by name, tested through its tool's real entry.
