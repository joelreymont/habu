---
title: "Fix bare-copy-lint-test's row on the deleted tools/tier.f"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:22:13.746114+02:00"
---

Problem: tools/lint/bare-copy-lint-test.f:93 asserts s" tools/tier.f" PATH-OWNED? but tools/tier.f no longer exists (deleted when tier 0 took over), so the case tests the lint's answer for a path that is not in the tree; found by the tools prune merge, 2026-10-30. Acceptance: the case names a file that exists and is owned, or the owned-path rule is asserted through a fixture the test creates; the lint row stays green. Files: tools/lint/bare-copy-lint-test.f, tools/lint/bare-copy-lint.f. Verify: hb --load tools/lint/bare-copy-lint-test.f rc 0 and the asserted path exists in the tree. Depends: none. Ownership: tools/lint/bare-copy-lint-test.f. Claim: unassigned.
