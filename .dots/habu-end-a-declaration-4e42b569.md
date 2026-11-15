---
title: "End a declaration's name read at its line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:45:20.652721+01:00"
---

Problem: a declaration's name read crosses lines in the load and tools/check.f: 'NEWTYPE' then 'NEWTYPE ok n' on the next line is rejected as a bad name 'NEWTYPE', and 'SUMTYPE' then ' VARIANT one ...' names the sum VARIANT, so the keyword-placed missing-name record appears only at the end of input. Acceptance: a declaring keyword alone on its line is rejected as a missing name at the keyword, in the load and check.f. Depends: none. Claim: unassigned.
