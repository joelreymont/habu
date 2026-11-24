---
title: Find a host word where its binding comes from
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T18:44:36.665479+01:00"
---

Problem: HOST-WORD? (src/habu/interpreter.f:2928) looks a host word up in hb's global wordlist and the open package's private wordlist, so a tree that exports a word into another package (EXPORT CHECK:INTRINSIC in FFI, src/lib/ffi-callback.f:557) fails to build with the installed hb (E-UNDEFINED INTRINSIC). Acceptance: hb finds its word where the target's binding comes from; that export builds with no stage. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-hostword.
