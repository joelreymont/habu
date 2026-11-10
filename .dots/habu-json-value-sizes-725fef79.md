---
title: "JSON:VALUE= sizes its scratch by commas and colons inside strings"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:52:43.562627+02:00"
---

Problem: the JSON:VALUE= review. TALLY (src/lib/json/equal.f, PUNCTUATION's count) counts every comma and colon in a text, those inside strings too, so the rows and slots are larger than the values need; a text with tens of MB of commas inside strings maps scratch in proportion and on Wasm would throw E-MEM-MAP though the values are few. Answers are right; only the size is off. Acceptance: the count skips string bodies (escapes honoured) at no second pass over the text, or the header states the bound and why it stands; a test compares a string full of commas. Files: src/lib/json/equal.f, src/lib/json-read-test.f. Verify: hb --load src/lib/json-read-test.f; the per-row cost before and after. Depends: none. Ownership: equal.f. Claim: unassigned.
