---
title: Define private layouts once
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.726409+03:00"
---

Remove self-assertions and exclusively consumed metadata in type-schema.f, type-family.f and checker private record layouts; engine-suite mirrors too. Keep constants/accessors as single source, actual target width/serialized ABI validation and storage/recovery tests. Update the docs/forth.md "Structures And Enums" prescription. Existing core behavior tests and integrated matching native gate.
