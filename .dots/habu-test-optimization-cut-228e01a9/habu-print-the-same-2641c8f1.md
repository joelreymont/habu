---
title: Print the same-host verdict before owners
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T07:07:15.045077+02:00\""
closed-at: "2026-10-01T12:55:46.197269+02:00"
close-reason: Fixed by nlzmpoul 337dacf1 (r4-small lane, reviews ACCEPT incl. 130)
---

Problem: tools/two-generation-core.f TG-SAME-HOST (:390-392) runs 1 TG-OWNER 2 TG-OWNER before it prints 'two-gen: same-host builds differ' and throws TG-FAIL-RC. Any throw from TG-OWNER (TG-MAP-NAME$ short read E-FS-IO from 7ae36737, FILE-SIZE E-FS-STAT, READ-ALL E-FS-OPEN/E-FS-IO/E-FS-CAPACITY, TG-MAP-RESERVE) leaves an unterminated owner line, loses the verdict, and MAIN (:422-423) turns it into a silent exit 67 (s" " code die prints nothing), so the operator sees neither the verdict nor the failure's name (found by the r4-small review of 7ae36737). Acceptance: the verdict line prints before any owner lookup; an owner-lookup failure prints a line naming it (the way tools/image-names.f:63-64 names a short read) and the run still exits TG-FAIL-RC; a forced owner-lookup failure in a scratch copy shows both lines and rc 1. Files: tools/two-generation-core.f. Verify: tools/two-generation-test.f, the forced scratch run. Ownership: two-gen same-host verdict order.
