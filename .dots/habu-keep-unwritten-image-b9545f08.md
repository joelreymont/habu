---
title: Keep unwritten image data off anonymous memory
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:19:34.723866+02:00"
---

Problem: a built program's DATA window is an anonymous private mapping (src/os/linux/sys.f:16 MAP-DATA-RESERVE); in an idle program built with Habu (Omabox's daemon) it holds 544 kB resident, 306 kB of it written, though the program changes little of its data after start. Acceptance: data a built program does not write after start costs no anonymous memory, and the idle program's DATA window holds only what it wrote, measured. Depends: none. Claim: unassigned.
