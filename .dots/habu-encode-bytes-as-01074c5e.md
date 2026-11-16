---
title: Encode bytes as a web form
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T18:29:47.279593+01:00"
---

Problem: Omabox's Dropbox OAuth URL and token bodies are application/x-www-form-urlencoded as Rust's form_urlencoded writes them (ASCII letters, digits and *-._ kept, space as +, every other byte as upper-case %XX); src/lib/uri.f encodes only file paths, so Omabox carries two private encoders. Acceptance: URI:FORM-ENCODE (Joel 2026-11-17) writes a byte string so encoded into a span and answers the count, as PATH>FILE does; a test checks every byte value against that rule. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-form.
