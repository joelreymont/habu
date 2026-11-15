---
title: Convert bytes to UTF-8 lossily
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T20:22:45.403655+02:00"
---

Problem: Habu cannot turn bytes that may not be UTF-8 into UTF-8 as Rust's from_utf8_lossy does, which a program writing paths into JSON needs (omabox docs/habu-rewrite.md, Habu libraries). Acceptance: UTF8:LOSSY replaces each maximal ill-formed subpart with U+FFFD, matching from_utf8_lossy on a corpus that includes truncated, overlong, surrogate and out-of-range sequences; its test. Depends: none. Claim: unassigned.
