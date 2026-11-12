---
title: "A caller bind at load time may write the build host's libc address into the target"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T19:09:01.238467+02:00"
---

Problem: src/lib/task.f CALLER-BIND (:1603) stores pthread_getspecific's address and the key into main-region header cells (CB-TLS-GET, CB-TLS-KEY, src/habu/layout.f:545-546). Not checked: what a build writes for those cells when a loaded file binds a caller slot at its top level; the linker compares header cells, so the host's address could land in the target or make two builds differ across boots. At run time every bind stores both before the owner, so a stale value is not read. Acceptance: a file that binds a caller slot at load time builds to the same bytes in two processes, and the target's two cells are zero; or the reproducer shows they already are, and the dot closes with the word that makes them so. Depends: none. Claim: unassigned.
