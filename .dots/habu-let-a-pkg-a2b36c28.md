---
title: "Let a package's part declare a public SUMTYPE or ENUM"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T21:21:09.338274+01:00"
---

Problem: a public SUMTYPE declared in a part of a package dies 'xref: generated declaration visibility mismatch' (rc 76), and a public ENUM there is rejected 'name is reserved or already taken' (seen by the items review, same on master's engine). Acceptance: both declare and their words are callable from outside, as tests; or, if the language forbids them, a Rust-style rejection says so. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-partdecl.
