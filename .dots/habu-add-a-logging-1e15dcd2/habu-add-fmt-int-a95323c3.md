---
title: "Add FMT:INT$ for a number's decimal text"
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T18:57:38.565392+02:00"
---

Logging lane 1. Brief: carl-dots/.out/carl-file/brief-log1-fmt.md. Acceptance: FMT:INT$ ( n -- ptr u8 n ) in src/lib/fmt.f, fmt-test.f cases incl. STR-MIN-I64 and SB untouched, rebuilt engine rc 0. Files: src/lib/fmt.f src/lib/fmt-test.f. Verify: bin/hb --load src/lib/fmt-test.f on the rebuilt engine. Depends: none. Ownership: carl. Claim: agent=worker workspace=.jj-ws.noindex/carl-fmt
