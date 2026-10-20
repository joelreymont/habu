---
title: Define private layouts once
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.726409+03:00"
---

Remove self-assertions and exclusively consumed metadata in type-schema.f, type-family.f and checker private record layouts; engine-suite mirrors too. Keep constants/accessors as single source, actual target width/serialized ABI validation and storage/recovery tests. Update docs/type-families.md prescription. Existing core behavior tests and integrated matching native gate.
