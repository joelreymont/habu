---
title: Give die a status, never a thrown code
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:54:12.281191+01:00"
---

Problem: die's last argument is the exit status, and 21 sites pass a negative thrown code: src/lib/xml/state.f:79, src/lib/xml/c2.f:63, src/lib/aio.f:171,325,326,379,382,673, src/lib/aio-macos.f:44,80,90,157,188, src/lib/task.f:155,158, src/lib/net/curl.f:785,1369, src/lib/secret/linux-test.f:87, tools/build-profile.f:71,73,74. The engine exits UNCAUGHT-RC (67, src/habu/layout.f:187) for a status outside 0..255, so the code is lost and the status says only that a throw went uncaught (probe /tmp/carl-ds/d-9212.f: rc 67). Acceptance: each site passes a sysexits status (70 for a broken invariant, 74 for I/O), and a code only die used is deleted. Depends: habu-give-every-thrown-0f75f834. Claim: unassigned.
