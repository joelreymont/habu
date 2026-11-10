---
title: "Cut the linker's long comments to two lines"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T20:54:10.129091+02:00"
closed-at: "2026-11-10T23:37:09.196074+02:00"
close-reason: "landed in 'Describe builds by part and word'; suite 273 of 273 and the measure green on the landing tip (Maki, Zen, Tender warm equal to cold)"
---

Problem: 81 comment blocks the incremental-builds stack touched in src/habu/linker.f run past two lines (forth-card 9; feature review finding 4). Acceptance: each is one or two true lines; the three saved-format tables stay. Files: src/habu/linker.f comment lines only. Verify: the file without comment lines is unchanged. Brief: .out/briefs/c-cut-comments.md. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-c1 and .jj-ws.noindex/carl-c2.
