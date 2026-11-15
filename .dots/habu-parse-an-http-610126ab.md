---
title: Parse an HTTP date
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T16:27:26.933736+02:00"
---

Problem: DATE (src/lib/date.f) parses no HTTP-date, so a Retry-After header in date form cannot be read (omabox's Dropbox client needs it for parity with the Rust version). Acceptance: DATE:PARSE-HTTP ( ptr u8 n -- option<n> ) answers epoch seconds for the IMF-fixdate, RFC 850 and asctime forms RFC 9110 5.6.7 says a recipient must accept, and none for anything else. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-parse-an-http-610126ab.
