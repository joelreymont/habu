---
title: "Make F64-TEXT's first-call setup run once"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T14:50:32.174892+02:00"
---

Problem: src/lib/f64-text.f INITIALIZE (:85-90) and REGISTER-CLEANUP (:77-81) test and then set C-LOCALE and REGISTERED with no lock, while the header (:5-6) states that the first call makes the C locale. When two tasks make their first PARSE or FORMAT at once, each runs newlocale: one locale leaks and CLEANUP-NATIVE is registered twice (IMAGE-LIFECYCLE:REGISTER locks, and the cleanup is idempotent). No answer is wrong and nothing crashes, since both locales stay valid until the image is prepared. Found from Maki by reading (tim, 2026-11-09), not observed at run time: Maki's import_step tool parses k_factor with F64-TEXT:PARSE on HTTP worker tasks (maki src/project-import.f:139). Acceptance: concurrent first calls make one locale and register one cleanup, and the header states how. Files: src/lib/f64-text.f. Verify: src/lib/f64-text-test.f. Depends: none. Ownership: src/lib/f64-text.f. Claim: unassigned.
