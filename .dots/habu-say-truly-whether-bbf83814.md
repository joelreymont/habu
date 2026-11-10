---
title: Say truly whether the build strips private words from hb
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T22:48:08.064052+02:00"
---

Problem: docs/architecture.md:17 and :90 say a word private to the system package is stripped from the delivered hb; a dots-cleanup worker read the tree and reports that is not true of hb now (not verified by the lead). Acceptance: the two sentences say what the delivered hb holds, checked against the build (tools/hb-build.f and what a delivered hb answers for a private system word), or the dot closes with the evidence that they are true. Files: docs/architecture.md. Verify: first, the check above; then read the changed lines against it. Depends: none. Ownership: unassigned. Claim: unassigned. Consumers: met by none.
