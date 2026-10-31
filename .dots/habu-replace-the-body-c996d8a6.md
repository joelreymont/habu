---
title: Replace the BODY-CLOSE-OFF copy with its name
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T02:17:43.035897+02:00"
---

Problem: tools/native-definition-cache.f:44 carries a literal $5A0 copy of CHECKER-OWNER-EXT:BODY-CLOSE-OFF, and a second BODY-CLOSE at :1865, because the installed hb that compiles the cache tool predates the field (the name resolves against the host's baked copy: E-UNDEFINED on the public hb); the copy moved from $580 to $5A0 during a rebase by hand and nothing checks it (comma lane 2 review, finding 1). Acceptance: in the first commit built by an installed engine that bakes the field, the literal is replaced by the name and the second BODY-CLOSE is dropped or justified by a fact; tools/check.f on the file and test/cell-body-e2e.f pass. Files: tools/native-definition-cache.f. Verify: tools/check.f -- tools/native-definition-cache.f; test/cell-body-e2e.f; test/cell-body-image-e2e.f. Depends: the comma lane landed and installed. Ownership: tools/native-definition-cache.f. Claim: unassigned.
