---
title: Speak HTTPS without libcurl
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T16:33:47.104586+02:00"
---

Problem: Habu's CURL (src/lib/net/curl.f) wraps the system libcurl, which loads about 33 libraries for protocols a client like Omabox never uses (HTTP/3, SSH, Kerberos, IDN, brotli, zstd). In Omabox's idle daemon they hold 1.8 MB of relocated pages, and libcurl's setup and kept connections 0.6 MB more, against the user's target of a megabyte or two. Acceptance: a Habu HTTPS client whose connection lifecycle is an FSM table checked against a TLA+ model, over OpenSSL's libssl and nothing else from libcurl; it covers what Omabox's src/dropbox.f asks of CURL today. Depends: none. Claim: unassigned.
