---
title: Refuse an HTTP status outside 100-599
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T18:56:52.327273+02:00"
---

HTTP:STATUS! accepts any number, so a 4-digit status or one under 100 writes a malformed status line (RFC 9110 15: a status code is three digits, 100-599). Found landing 04b30d84 (REASON$ per the IANA registry). Refuse at STATUS! with the HTTP block's code for an invalid argument, with an http-test row that fails before. Owner: carl.
