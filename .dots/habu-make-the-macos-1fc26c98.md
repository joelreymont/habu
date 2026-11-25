---
title: Make the macOS AIO run state scratch
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:58:10.379256+01:00"
---

Problem: src/lib/aio-macos.f keeps FD-ROWS, FD-CAP, FD-DISTINCT, MAC-POLL-COUNT, MAC-READ-FD and MAC-WRITE-FD in a program, beside MAC-POLLS, a null pointer, as the Linux file did. Acceptance: a load that ran AIO on macOS leaves none of them in the program, by the test/file-objects.f calls sequence on a Mac. Depends: none. Claim: unassigned.
