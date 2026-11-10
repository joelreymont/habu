---
title: "Make F64-TEXT's first-call setup run once"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T14:50:32.174892+02:00"
---

Problem: src/lib/f64-text.f INITIALIZE (:65-67) tests C-LOCALE, a scratch cell (:33), then calls newlocale and stores the result with no lock, while the header (:4-6) says the first call makes the C locale and later calls only read it. When two tasks make their first PARSE or FORMAT at once, each runs newlocale and one locale leaks. No answer is wrong and nothing crashes, since both locales stay valid. Found from Maki by reading (tim), not observed at run time: Maki's import_step tool parses k_factor with F64-TEXT:PARSE on HTTP worker tasks (maki src/project-import.f:139). The cleanup registration this dot also named went in db0744e1.
Acceptance: concurrent first calls make one locale, and the header states how.
Files: src/lib/f64-text.f, src/lib/f64-text-test.f.
Verify: bin/hb --load src/lib/f64-text-test.f, with a case where several tasks make their first call at once and one locale results.
Depends: none. Ownership: src/lib/f64-text.f. Claim: unassigned.
