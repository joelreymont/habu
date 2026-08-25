---
title: Migrate wordlist visibility tests
status: closed
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:49.280427+02:00"
closed-at: "2026-07-30T05:34:44+02:00"
close-reason: The XREF owner marker and native publication guard preserve owner authority without removing checked wordlist effects, so these test migrations have no product result.
---

Why: tests that inspect raw wordlist identifiers must assert public/package visibility through compiler and XREF behavior instead. Result: replace only those test assertions and fixtures; retain hostile saved-WID bypass negatives. Owner: existing wordlist/package visibility tests only. Production red: deleting primitive effects leaves tests coupled to forbidden internals. Acceptance: public/private/reopen visibility and saved-WID bypass are observed through production compiler entry points; no test helper publishes or compares raw WIDs. Forbidden: copied resolver, substring test, lint, runtime shim, or weakened negative. Smallest owning check: focused package/XREF visibility suite.
