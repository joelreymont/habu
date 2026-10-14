---
title: Locate a missing undefine name in check.f
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T11:32:12.017139+02:00"
closed-at: "2026-10-15T01:13:03.732314+03:00"
close-reason: "Completed by master ancestor 919b6512: nameless undefine is located in every check mode, changing the reproduced unlocated exit74 to the existing located checker exit70. Current implementation and recorded regression checks verified."
---

Problem: after r4-lexrec commit 1 (dot 629efa23) check.f refuses a definer with no name at the definer (E-MISSING-NAME), but 'undefine' at end of input, a name reader, is refused by preverify as 'verify-source: missing undefine name', rc 74, with no location. Acceptance: the same located E-MISSING-NAME refusal and rc 70 as the definers, in every check.f mode; a case seen to fail first. Files: tools/check-core.f, src/habu/verify-source.f, tools/check-test-lib.f.
