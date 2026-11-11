---
title: The SLOT-TARGET craft is refused by the span check, not the does> target check
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:59:02.527263+02:00"
---

Problem: found by the link-crafts worker (lane commit 99d2b1ab, dot habu-refuse-a-slot-2eee435e), inferred, not traced: test/fixtures/file-objects/build.f SLOT-TARGET grows the object by 24 bytes without moving the TIMES spans, so its result 1 probably comes from EXTENT-CK or OWN-IDS! refusing the stale spans and not from the does> target check it is there to test. The lane's two new crafts showed exactly this until they moved the spans (CRAFT-GROW). Acceptance: SLOT-TARGET grows through CRAFT-GROW; with the does> target check removed in a scratch build the craft prints 0, with it present 1. Files: test/fixtures/file-objects/build.f. Verify: test/file-objects.f on a built hb, and the scratch build once. Depends: habu-refuse-a-slot-2eee435e (CRAFT-GROW arrives with it). Ownership: the fixture. Claim: unassigned.
