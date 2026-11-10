---
title: Give each word its own time in the object
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:35:50.073296+02:00"
---

Problem: src/habu/linker.f saves one time per file piece (COMPILED-TIME :2805), so a dependent cannot tell which word of a piece changed. Acceptance: the object has a TIMES section in its head holding, per piece and word (each record PIECE+ writes), a time kept when the word's entry, emission and facts text compile to the old object's bytes, and their spans; a build keeps the times by record index; nothing checks them yet; an unused word appended to repl.f leaves every other word's time, a body edit renews that word's alone; unsigned warm equals cold; test/file-objects.f passes. Files: src/habu/linker.f. Verify: brief .jj-ws.noindex/carl-layout/.out/briefs/lane-word-times.md Evidence. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-wt
