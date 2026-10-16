---
title: Remove unused compiler digest protocols
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.642448+03:00"
---

Delete test-only NTAPE DIGEST/VERIFY, IR-SCHEMA DIGEST/TABLE-DIGEST/VERIFY, native-effect and binding encoding/digests, CTARGET/CNUM DIGEST. IR-SOURCE source SHA fields are computed and copied without a production decision; retain source identity/spans/origins. Keep target/policy ENCODE while context calls it. Trace callers and preserve genuine compiler verifiers; focused existing suites, integrated native gate.
