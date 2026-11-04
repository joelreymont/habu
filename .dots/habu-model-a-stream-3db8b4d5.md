---
title: "MODEL: a stream that ends before its response is a dropped call"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:09:21.979909+02:00"
---

From stuart (Tender corpus, MODEL at 549c7df5), 2026-11-05. Problem: src/lib/net/model.f:2294 FOLLOW ends a call E-RESPONSE when the provider closes the stream cleanly before a terminal event (seen at 3630 s with last status in_progress); DECISIONS 'long model calls' (2026-11-04) says a dropped connection ends the call E-TRANSPORT, which Tender retries, and a stream that ends before its response ends is a dropped call. Acceptance: FOLLOW throws E-TRANSPORT for a stream that ends without its terminal event, its comment says so, and the no-progress test (model-test.f) expects E-TRANSPORT; a terminal failed/incomplete event keeps its own error. Files: src/lib/net/model.f, src/lib/net/model-test.f, src/lib/net/test-model.f if its stream helpers need a cut-short reply. Verify: model-test.f, test-model-test.f. Depends: none. Ownership: carl. Claim: unassigned.
