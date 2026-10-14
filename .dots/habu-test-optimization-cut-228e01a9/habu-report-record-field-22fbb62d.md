---
title: Report record field errors as check diagnostics
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T05:24:08.657569+02:00"
closed-at: "2026-10-15T01:13:03.597097+03:00"
close-reason: "Completed by master ancestor bfecf2d8: bad record fields become located diagnostics and checking continues. Commit records failing-before and passing-after checker and gate cases; current implementation verified."
---

Problem: in tools/check.f a VALUE-RECORD with a bad field type dies inside CHECKER-DEFRECORD with the loader's message (rc 70), so the CHK-E-CHECK catch in CHK-VREC-DEFRECORD (tools/check-core.f near :813) can never fire: check.f stops at the first such record without a located diagnostic and drops every later finding. Found by the r4-nomname lane. Acceptance: a bad field type gives check.f a diagnostic that names the record field and its line, the check continues to later findings and exits 70, the loader's message and rc for the same source are unchanged, and no unreachable catch remains; cases through tools/check-test-lib.f written before the code. Files: tools/check-core.f, src/core/checker.f if the registration must report instead of die, tools/check-test-lib.f. Verify: tools/check-test.f, the typed-storage and record loader suites. Depends: habu-give-deflinear-and-7498dc6d. Ownership: check.f handling of record registration failures.
