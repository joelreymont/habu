---
title: Store the validated compiler binding directly
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.662814+03:00"
closed-at: "2026-10-22T12:36:35.339242+03:00"
close-reason: "Triage 2026-10-22, DONE: context.f:448 BINDING@ reads HEADER-BINDING directly; 3bdffeeafa17 deleted CODES-*/STAGE-*/N>ARCH… decoders."
---

ir/context.f serializes and decodes CBIND binding inside one transient process and reconstructs it on every BINDING@; session/emission.f already stores the typed value. Store one validated binding, preserve lifetime/stale-handle protection, delete wire roundtrip and three never-bound service slots. Remove target/policy encoding only after no real consumers remain. Existing context lifetime and compiler consumer tests; coordinate digest deletion.
