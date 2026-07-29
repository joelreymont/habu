---
title: Migrate wordlist visibility tests
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:49.280427+02:00"
---

Why: tests that inspect raw wordlist identifiers must assert public/package visibility through compiler and XREF behavior instead. Result: replace only those test assertions and fixtures; retain hostile saved-WID bypass negatives. Owner: existing wordlist/package visibility tests only. Production red: deleting primitive effects leaves tests coupled to forbidden internals. Acceptance: public/private/reopen visibility and saved-WID bypass are observed through production compiler entry points; no test helper publishes or compares raw WIDs. Forbidden: copied resolver, substring test, lint, runtime shim, or weakened negative. Smallest owning check: focused package/XREF visibility suite.
