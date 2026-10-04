---
title: Give aot-call-report-lib.f a package
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T20:18:16.890541+03:00"
---

Found by lane 507 (2e9d3970, 126b0ffb): tools/aot-call-report-lib.f declares no package, so about 60 words (OUT-C, BL?, JSON-NUM, LE32@, ...) are global in every gate that loads it (test/gate-aot-image.f, test/gate-aot-positive-lib.f, test/gate-aot-positive-preseed.f, tools/aot-call-report.f, tools/aot-call-report-test.f). Card section 1 and CLAUDE.md: every module has a real package; public effects keep meaningful types. Acceptance: the library is one package with a public surface of the words its callers use, the rest private; callers qualify or use it; check.f on each file rc 0; tools/aot-call-report-test.f and test/gate-aot-positive-bundle.f rc 0. Base: after 126b0ffb lands (same file).
