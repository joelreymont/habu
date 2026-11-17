---
title: Reject control characters in Basic credentials
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:35:50.558882+01:00"
---

Problem: RFC 7617 section 2 says a Basic user-id and password MUST NOT contain control characters (RFC 5234 CTL); HTTP:BASIC-CREDENTIALS (src/lib/net/http-basic.f) encodes them, so a password read with its trailing newline gives credentials the server refuses with no diagnostic. Acceptance: a control character in the user-id or the password is a rejection before a byte is written, with a test. Depends: Joel's name for the password's code. Claim: unassigned.
