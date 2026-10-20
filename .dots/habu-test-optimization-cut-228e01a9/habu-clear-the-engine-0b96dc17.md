---
title: Clear the engine path from the hash context before capture
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T14:50:44.794623+02:00"
closed-at: "2026-10-14T19:56:27.932087+03:00"
close-reason: e9df93aa clears every engine-id query buffer before capture; engine-id-capture-e2e observed the pre-fix leak and now passes, with g1/g2 convergence.
---

Problem (lane 341 r4-engroot, d1099dfb): after KEY$ runs on macOS, src/lib/engine-id.f OPEN-RUNNING (:129-131) leaves the engine's path in EID-FSHA-CTX's path field, and CLEAR-CACHE (:70-79) does not clear it, so an image captured after KEY$ could carry the build host's engine path. Acceptance: show whether KEY$ can run before a capture in any build path (and the path reaches the image), then clear the field in CLEAR-CACHE (engine-id-capture test asserts the captured bytes hold no path), rebuild if baked, g1 == g2. Files: src/lib/engine-id.f, test/engine-id-capture-e2e.f.
