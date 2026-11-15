---
title: Act on names under a directory descriptor
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T20:22:45.375532+02:00"
---

Problem: src/lib/fs-anchored.f only looks names up; a program that must never follow a symlink under its root needs to open, create, rename, remove and stat by one name relative to a directory descriptor (omabox docs/habu-rewrite.md, Habu libraries). Acceptance: fs-anchored.f opens a directory, a file to read or write, or any entry by O_PATH, makes an O_TMPFILE and names it by linkat, renames with and without replace, removes a file or directory, makes a directory and stats, each on a name checked as OPEN-DIR-AT checks one, with O_NOFOLLOW, answering a bool or option with errno in FS:ERRNO; its test beside it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-anchored.
