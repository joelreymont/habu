---
title: Count a reply cut in its headers as lost
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T13:31:22.607219+02:00"
---

Problem: a peer that sends a status line and closes before the blank line (HTTP/1.1 200 OK CRLF, then close) gets CURLE_OK from libcurl, and src/lib/net/curl.f then throws E-RESULT (STATUS-TRY at 1071 and 1959, FINAL-BLOCK at 1899) instead of answering a failed transfer, so a client crashes on a peer's bytes instead of retrying. Found by omabox's delete lane. Acceptance: such a reply answers failed with a code CURL:LOST? accepts, through PERFORM, AWAIT and a stream's end, and HEADERS never throws on a peer's bytes. Depends: none. Claim: unassigned.
