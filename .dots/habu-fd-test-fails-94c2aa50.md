---
title: fd-test fails cases 30 and 33 on a 16 KiB-page kernel
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T21:32:01.708623+01:00"
---

Problem: on Arch Linux ARM (Asahi, kernel 7.2.11, getconf PAGESIZE 16384, pipe-max-size 1048576), src/lib/fd-test.f fails assert cases 30 and 33 (expected true got false) on master 76bbcbfe; spark (4 KiB pages) passes. Acceptance: the cause is named as path:line and fixed where it lies; the fd suite passes on 4 KiB and 16 KiB pages. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-h16fd.
