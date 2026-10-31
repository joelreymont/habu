---
title: Reuse files compiled under an app hook
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T11:38:20.020123+02:00"
---

Problem: a file compiled while an app-installed check hook judges (set-check of a hook defined outside the engine) is never stored: the pinned host checker role cannot name that hook, so CAPTURE-CHECKER-ROLE orders it (checker-producer-unknown; tools/native-definition-cache.f CHECKER-ROLE$ after the habu-key-a-file-b9c53392 fold-in). Same cost as master's CHECK-STATE 2 refusal; no stale reuse. Following the hook needs a role that digests the host role plus the session's sources read up to the hook change, which BUILD-PRODUCER cannot start without a new public word. Acceptance: an app build whose files load after its own hook reuses them warm, and a hook edit or removal still misses exactly the files compiled under it (warm == fresh cold). Depends: the producer-identity work (identity-design.md: engine read log in SOURCE-PRODUCER). Files: tools/native-definition-cache.f, src/core/source-producer.f. Verify: test/native-file-frame-e2e.f hook case. Claim: unassigned.
