---
title: "Answer a JSON value's whole byte extent from the reader"
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T13:04:48.667593+03:00"
---

Problem (aspen, Tender TED adapter): lib/json-read.f JR:SPAN$ answers the CURRENT TOKEN's bytes (TOK-AT-OFF/TOK-LEN-OFF), so at an object's '{' it answers one byte; a consumer that needs the bytes of one object out of a larger document (notices[i] as the tender's notice) cannot take the difference of two token pointers in checked code, and the private RAW-SPAN$ (line 472) is the same token span. Acceptance: a public JR:VALUE-SPAN$ ( JR:reader -- JR:reader ptr u8 n ) that answers the bytes of the value the reader stands on - a scalar's own token, or an object/array from its opening bracket through its matching closing bracket - and leaves the reader positioned after that value exactly as SKIP-VALUE does (implemented as: remember TOK-AT-OFF, SKIP-VALUE, answer source + start for POS-OFF - start; integers, no pointer subtraction; nested and escaped-string contents pass through untouched). Fixtures in lib/json-read-test.f: an object inside an array, a nested object, a scalar, an empty object, a malformed value refusing as SKIP-VALUE does; docs/stdlib.md beside JR:SPAN$. Files: lib/json-read.f, lib/json-read-test.f, docs/stdlib.md. Verify: bin/hb --load lib/json-read-test.f; the json rows. Depends: none. Ownership: lib (alder). Claim: unassigned.
