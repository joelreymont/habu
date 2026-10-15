---
title: Review and schedule the package-build design
status: closed
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.843356+03:00"
closed-at: "2026-10-15T03:33:05.905138+03:00"
close-reason: "Superseded design/scheduling bookkeeping: f90e2828255b and 03fcbb1cf362 implemented FILE units and application caching, documented in native-file-object.md. Current reuse/maker and shared-engine gaps are owned by 1d1bf6f8 and 68ed6b51, not a new historical Fable review campaign."
---

Problem: PA-r2 §8, §23 and P4 rest on P1 (docs/package-build.md after habu-land-the-portability-dbde2246), whose baseline 37b2c1b7 is 608 commits behind master; P1 was reviewed only at its PA-r2 interface. Acceptance: a Fable review of docs/package-build.md against master decides what P4 implements first; P4's child dots filed (source-position importer, observation hooks, .hbp profile, C01-C18). Files: docs/package-build.md, .dots/. Verify: the review report; the child dots pass worker-preflight. Depends: habu-land-the-portability-dbde2246, habu-make-cross-target-934166cc. Ownership: docs/package-build.md. Lane: dave, who also decides P4's timing; it is a Habu build-speed campaign Maki does not need. Claim: unassigned.
