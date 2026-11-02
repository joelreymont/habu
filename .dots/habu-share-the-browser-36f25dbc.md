---
title: "Share the browser suites' host-cli and Chrome helpers"
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T18:28:14.831178+02:00"
---

Problem: the browser test suites (test/browser/*-test.f, src/lib/browser/*-test.f) each copy host-cli driver words, FIND-CHROME, the Chrome launch and stopGroup (tim's H2 reviews: H2a F5, H2b F9). Acceptance: one test support file holds them, each suite requires it, no copy remains; every browser suite still passes. Files: the browser suites. Verify: each suite tim ran for H2 (src/lib/browser/app-test.f, test/browser/app-test.f, request-test.f, display-test.f, src/lib/browser/host-test.f). Depends: H2 lands (tlxysuoq, susvtroy). Ownership: unassigned. Claim: unassigned.
