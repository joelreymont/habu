---
title: Remove redundant unit tests
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-24T17:24:46.362037+02:00\\\"\""
closed-at: "2026-09-24T17:57:22.587291+02:00"
close-reason: "Removed 14 redundant test files and 4 unused helpers; pruned implementation mirrors across mixed suites (over 10,000 net lines). Added the requested E2E-first testing rules to AGENTS.md. Four parallel audit lanes integrated and independently reviewed, retaining unique edge and failure cases. Fresh native product build passed and remained byte-identical to the installed engine; full macOS gate passed 490/490 with exit 0. Two saved-image round trips passed and produced identical artifacts. Evidence, logs and repeatable artifacts: /Users/joel/.cache/tmp/habu-test-prune/."
---

Audit compiler, engine/checker, libraries and tooling tests against existing end-to-end coverage. Delete isolated tests and test-only helpers that add no concrete bug detection beyond those flows; retain isolated checks only for identified failures absent from E2E. Update suite registration and direct references, add the user’s three test-policy rules to AGENTS.md, and retain verifiable repeatable E2E artifacts. Integrator owns cross-lane review, integrated native gate, commit/push and workspace cleanup. Record each deletion’s coverage witness before landing; do not weaken retained assertions or add post-hoc unit tests.
