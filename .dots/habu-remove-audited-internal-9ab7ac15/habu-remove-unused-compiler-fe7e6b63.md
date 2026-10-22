---
title: Remove unused compiler digest protocols
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.642448+03:00"
closed-at: "2026-10-22T12:36:35.201506+03:00"
close-reason: "Triage 2026-10-22, DONE: 3bdffeeafa17 removed the context codes/stage encoders (context.f -283), schema/attr digests (schema.f -250, attr.f -1683). No DIGEST remains in tape.f, schema.f, target.f or numeric-policy.f."
---

Delete test-only NTAPE DIGEST/VERIFY, IR-SCHEMA DIGEST/TABLE-DIGEST/VERIFY, native-effect and binding encoding/digests, CTARGET/CNUM DIGEST. IR-SOURCE source SHA fields are computed and copied without a production decision; retain source identity/spans/origins. Keep target/policy ENCODE while context calls it. Trace callers and preserve genuine compiler verifiers; focused existing suites, integrated native gate.
