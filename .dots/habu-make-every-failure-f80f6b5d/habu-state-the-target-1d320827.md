---
title: State the target of every OS-only file
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:12:46.179024+02:00"
---

Problem: a file that builds only for some targets states them on its first code line with ?TARGET (src/core/include.f), but the files that build for one OS only (src/lib/aio-macos.f, src/lib/secret/macos.f, src/lib/power/macos.f and the like) state nothing, so a require of one on the wrong OS fails some other way. Acceptance: a census lists every file under src that builds only for some targets and is not reached through a file that states them; each states its targets; a require of one on another target prints E-TARGET naming it. Depends: none. Claim: unassigned.
