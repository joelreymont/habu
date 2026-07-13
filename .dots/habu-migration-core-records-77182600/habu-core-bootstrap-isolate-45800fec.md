---
title: "Core bootstrap: isolate PTR-VARIABLE"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T18:00:18.070811+02:00\\\"\""
closed-at: "2026-07-14T00:49:30.827261+02:00"
close-reason: Landed PTR-VARIABLE in one-concern pointer-storage source/effect owners with native/recovery/pin/cache/diagnostic manifests and focused isolation/effect/round-trip coverage. Verified full native gate 70565ms/78400ms, Maki, PTX, no-binary bootstrap, typed-local, trust 665/691/0, host 0, filemap 723/0, dot-dep 433/319/0, parallel-agent 0; independent destruction review found no defects.
---

Move PTR-VARIABLE out of src/core/structures.f into an independent one-concern
pointer-storage owner with its effect/trust boundary, verify-source model,
FILEMAP/TRUSTED rows, and focused definition/fetch/store/recovery tests.
Preserve the `( -- ptr ptr a )` effect exactly. It must not retain +FIELD,
CFIELD:, STRUCT-BYTE+, STRUCT-ACTIVE, or any composite-layout parser.

Claim: agent=pointer_storage workspace=.jj-ws/type-dsl-ptr
