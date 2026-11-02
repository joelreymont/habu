---
title: Remove the duplicate PATH and READ in browser host.f and app.f
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T18:28:14.852916+02:00"
---

Problem: src/lib/browser/host.f and app.f each define a private PATH and READ doing the same thing (tim, H2 landing note 3). Acceptance: one definition at the owner the other already requires, the copy removed; both files' suites pass. Files: src/lib/browser/host.f, app.f. Verify: src/lib/browser/host-test.f, src/lib/browser/app-test.f. Depends: H2 lands. Ownership: unassigned. Claim: unassigned.
