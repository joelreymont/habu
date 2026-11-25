---
title: "Keep zeros out of a program's file"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:20:36.307446+01:00"
---

Problem: an ELF program carries its data as whole 64 KiB pages (src/habu/link-data.f PAGE-RUNS,), so zeros beside any non-zero cell lie in the file: 2.2 MB of tenderd's 3.5 MB of data at 512 bytes, 0.94 MB of hb's. Acceptance: a program's and an engine's file carries its data without those zeros, the boot places it as fast as today's on both machines, and the program's data is the same. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-x-page.
