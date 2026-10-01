---
title: "Refuse a missing closure member in EC:BUILD"
status: closed
priority: 1
issue-type: task
created-at: "\"2026-10-01T07:14:49.587367+02:00\""
closed-at: "2026-10-01T12:54:48.374478+02:00"
close-reason: Fixed by mtznmnrz 531df393 (r4-repl lane, review 127 ACCEPT)
---

Problem: tools/event-closure-lib.f EC-ENQUEUE (:110-111, 'a u FILE? 0= if exit then') and EC-DIR-QUEUE (:169-170) drop a required/included event whose resolved path is not a file, so EC:BUILD returns a closure without it and test/whitebox-key.f CLOSURE-CK+ (:64-69) keys fewer files than the build reads, against whitebox-key.f:62-63 ('the key never silently covers fewer files than the build reads') and BUILD-WITH's own contract ('a missing dependency is a refusal at that read'). Measured: entry 'require lib/present.f' + 'require lib/absent-dependency.f' gives DISCOVER:RUN 2 events, EC:BUILD COUNT 2 with no throw. This is why test/whitebox-manifest-child.f passed while sign-id.f was in no key (found by the r4-repl review of adda385c). Every member of the real tools/native-build.f closure has 0 loading events whose path is not a file, so a refusal is safe. Acceptance: EC:BUILD refuses a missing member at that read, as BUILD-WITH does; a case with a missing require written first and seen to return a short closure; every BUILD-based key (whitebox, build-fixpoint CHAIN-DIGEST!, fixture-writer, app-image-engine) still builds. Files: tools/event-closure-lib.f, its test. Verify: the new case, tools/source-discovery-test.f, test/whitebox-engine-key-test.f, the keyed rows that call EC:BUILD. Ownership: event closure completeness.
