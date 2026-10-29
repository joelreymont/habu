---
title: Decide the 1023-byte line cap in STEP
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T23:42:12.878088+02:00"
---

Problem: src/habu/stepper.f:90 STEP copies the rest of the input line into SBUF and silently cuts it to 1023 bytes (SLEN @ 1023 > IF 1023 SLEN ! THEN): a fixed cap on user content, found by the src/habu comment prune 2026-10-29; a cap on content needs Joel's decision. Acceptance: Joel's ruling recorded here, then either the cap gone (the step buffer sized to the line, or the step reads from the input directly) or the bound kept as a stated protective limit with its reason beside the code. Files: src/habu/stepper.f. Verify: a stepped line longer than 1023 bytes runs whole. Depends: none. Ownership: src/habu/stepper.f STEP and SBUF. Claim: unassigned.
