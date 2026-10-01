---
title: Refuse a short names-map read
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-01T04:12:40.693730+02:00\""
closed-at: "2026-10-01T12:55:45.517965+02:00"
close-reason: Fixed by wuylwuox 028db8c0 (r4-small lane, reviews ACCEPT incl. 130)
---

Problem: tools/two-generation-core.f:316 TG-MAP-NAME$ reads the .names file with READ-ALL and drops its byte count, so a short read is parsed as if the whole file arrived and a truncated names map yields wrong or missing names in the two-generation report without an error. Acceptance: a read that returns fewer bytes than FILE-SIZE refuses with a named error instead of parsing; shown through the real two-generation path or its test. Files: tools/two-generation-core.f. Verify: tools/two-generation-build.f (or its test). Depends: none. Ownership: TG-MAP-NAME$.
