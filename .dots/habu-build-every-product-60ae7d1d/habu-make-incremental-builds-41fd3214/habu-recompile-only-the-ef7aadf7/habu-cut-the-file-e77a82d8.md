---
title: Cut the file-objects test headers to two lines
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T21:20:50.838847+02:00"
closed-at: "2026-11-10T23:37:09.233718+02:00"
close-reason: "landed in 'Cut the file-objects test headers'; suite 273 of 273 and the measure green on the landing tip (Maki, Zen, Tender warm equal to cold)"
---

Problem: test/file-objects.f lines 1-144 and test/fixtures/file-objects/build.f lines 1-67 are header essays that narrate every build (forth-card 9; feature review finding 4). Acceptance: each header is two lines, each case word says what it guards in one or two lines, and a fact the code cannot say sits at its word. Files: the two files' full-line comments only. Verify: each file without comment lines is unchanged. Brief: .out/briefs/hc-cut-test-headers.md. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-hc
