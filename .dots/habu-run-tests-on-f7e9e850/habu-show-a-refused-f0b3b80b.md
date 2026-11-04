---
title: "Show a refused subject's output when SUBJECT:REFUSED? fails"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:59.573979+02:00"
closed-at: "2026-11-04T11:48:50.134933+02:00"
close-reason: "Census 2026-11-04: an acceptance case of habu-fork-only-to-59d3557e, written there. a failed REFUSED? showing the child's output must hold in SNIPPET after the subject library's spawn conversion"
---

Problem: src/lib/test/subject.f keeps the child's captured output private, so a failed SUBJECT:REFUSED? prints only the label: the reader cannot see what the subject printed or why it did not match. Found by the c2mem lane. Acceptance: a failing SUBJECT:REFUSED? prints the expected text, the exit code and the subject's captured stderr after its FAIL line; passing cases print nothing new. Files: src/lib/test/subject.f. Verify: a planted wrong expectation shows the child's text; the c2-memory-source row stays green. Depends: none. Ownership: subject.f. Claim: unassigned.
