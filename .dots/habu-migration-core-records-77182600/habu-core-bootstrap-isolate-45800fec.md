---
title: "Core bootstrap: isolate PTR-VARIABLE"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T18:00:18.070811+02:00\""
---

Move PTR-VARIABLE out of src/core/structures.f into an independent one-concern
pointer-storage owner with its effect/trust boundary, verify-source model,
FILEMAP/TRUSTED rows, and focused definition/fetch/store/recovery tests.
Preserve the `( -- ptr ptr a )` effect exactly. It must not retain +FIELD,
CFIELD:, STRUCT-BYTE+, STRUCT-ACTIVE, or any composite-layout parser.

Claim: agent=pointer_storage workspace=.jj-ws/type-dsl-ptr
