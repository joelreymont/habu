---
title: Rename the subject test library
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T19:04:35.666278+02:00"
---

Joel 2026-10-28: SUBJECT (src/lib/test/subject.f) and its package get a name that says what they do (evaluate a source in a child process); hard cutover across its 109 users. Done inside the spawn conversion of the subject runner. SNIPPET (src/lib/test/snippet.f), approved by Joel 2026-10-28 (interview); RUN, EXITED?, REFUSED?, SHOW, TIMED-OUT, RC-TIMED-OUT keep their effects, plus SNIPPET:REQUIRE ( ptr u8 n -- ) and SNIPPET:START ( ptr u8 n ptr fd n -- pid ). Claim: unassigned.
