---
title: "USER-BAND cannot hold every library's task rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:28:08.578531+02:00"
---

From phil (FS:ERRNO$ lane), 2026-11-05, measured on the installed hb: USER-BAND is $5300..$67D0, 5328 bytes; an image that loads every library with TASK:+USER rows dies at src/lib/serial.f with E-TASK-USER (-3605), 16 bytes left after aio..process; task.f alone claims 2896; Zen's server claims 5080 (248 free; 184 after ERRNO$'s 64-byte FS-ABI buffer). The FS-MUT-ABI comment in src/habu/layout.f (USER-BAND's free bytes) is stale, and bootstrap/cg/forth.fs:508-511 lists band addresses by hand. Problem: a fixed per-task band sized by hand fails when programs combine libraries. Acceptance: needs a design (grow the band, or rows outside a fixed band); every library loads into one image; the hand-listed addresses derive from layout.f. Files: src/habu/layout.f, src/lib/task.f, bootstrap/cg/forth.fs. Verify: a test loading every TASK:+USER library together. Depends: none. Ownership: carl. Claim: unassigned.

Consumers: nothing of Zen fails today; the 5080 of 5328 bytes was measured on 2026-11-05 and not since.
