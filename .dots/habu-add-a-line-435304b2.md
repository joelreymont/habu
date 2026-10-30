---
title: Add a line diff and three-way merge library
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T17:24:59.424187+02:00"
---

Problem (tim, from Maki): Maki's proposal and draft lanes need a line diff with a git-style unified writer and a three-way line merge that reports conflicts; Habu has none. Requirement: tim's brief /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/chat-tools/brief-diff.md; suggested words DIFF:UNIFIED and DIFF:MERGE3 ( base bu ours ou theirs tu BUF -- bool ). Acceptance: src/lib/diff.f in package DIFF, end-to-end tests of the unified output and of clean and conflicting merges, carl's Opus review, landed by carl. Files: src/lib/diff.f, its test, docs. Verify: the library's test rows. Depends: none. Claim: agent=tim worker workspace=Habu .jj-ws.noindex/tim-diff (hand-in to carl).
