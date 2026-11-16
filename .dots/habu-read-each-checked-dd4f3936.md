---
title: Read each checked file once
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T07:20:50.947090+01:00"
---

Problem: tools/check.f reads a named file again after discovery accepted it: CHK-WRITE-SUBJECT (tools/check-core.f:809, READ-WHOLE-FILE), the default-mode reserved-name lint (:909, RNL-LOAD-SOURCE) and CHK-FILE-READ (tools/check-verify-core.f:284, through CHK-EDGE-ARM), outside any handler for file-system errors; a file that grows (E-FS-CHANGED) or is deleted (E-FS-OPEN) between two reads ends the check with rc 67 and no record. Acceptance: check.f reads each user file once and every later use takes those bytes; a file changed or deleted during a check gives one record and rc 74, never 67; a test pins it where a fixture can change a file between reads. Depends: none. Claim: unassigned.
