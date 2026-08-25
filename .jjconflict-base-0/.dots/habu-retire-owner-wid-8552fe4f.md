---
title: Retire OWNER-WID fixtures
status: closed
priority: 2
issue-type: task
created-at: "2026-07-30T06:09:46.223433+02:00"
closed-at: "2026-08-02T15:01:23.858023+02:00"
close-reason: "Completed by reviewed hard-cut ancestor a8c716c53cda322729f8e7d5c92a406f095dc094: all OWNER-WID-only fixtures and gate enrollment were deleted while protected-WID coverage remains."
---

Why: dedicated OWNER-WID fixtures and gate enrollment preserve tests for a production-empty registry that the hard cut deletes. Result: delete the owner-wid fixture files and their suite/run-list enrollment; remove only owner-specific cases from shared AOT, seal, coverage, and image tests while retaining every PROT-WID and generic package case. Do not touch production OWNER-WID code, layouts, readers, writers, primitives, or checker effects in this leaf. Owner: OWNER-WID-only tests and exact shared-test rows. Production red: the full gate still builds injected OWNER-WID artifacts whose only purpose is the obsolete registry. Acceptance: no runnable test or coverage row names an owner-wid fixture; shared suites remain green and still exercise PROT-WID; exact fixture inventory is empty. Forbidden: replacement test model, skip logic, compatibility fixture, lint, production edit, or unrelated test cleanup. Smallest owning check: gate-stdlib case inventory plus the touched shared suites. Claim: unassigned.
