---
title: "A caller bind at load time may write the build host's libc address into the target"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T19:09:01.238467+02:00"
---

Problem: src/lib/task.f CALLER-BIND stores pthread_getspecific's address and the key into main-region header cells (CB-TLS-GET, CB-TLS-KEY, src/habu/layout.f :758). Not checked (pg-notice review): what a build writes for those cells when a loaded file binds a caller slot at its top level; the linker compares header cells, so the host's address could land in the target or make two builds differ across boots. At run time every bind stores both before the owner, so a stale value is not read. Acceptance: a file that binds a caller slot at load time builds to the same bytes in two processes, and the target's two cells are zero; or the reproducer shows they already are, and the dot closes with the word that makes them so. Files: src/lib/task.f, src/habu/layout.f, src/habu/linker.f. Verify: two cold builds of the reproducer compared with cmp. Depends: habu-route-libpq-notices-a88cc555 landed. Ownership: the reproducer. Claim: unassigned.
