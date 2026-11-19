---
title: Bind free and realpath at load on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T10:13:14.477669+02:00"
---

Problem: src/habu/kernel-x64.f REALPATH-ROW (about :911) looks up free and realpath by dlsym on every call, though the shared writer binds both into GOT slots (src/os/linux/elf.f IMPORT$); arm64 loads them from their slots (dot a8fb553d). Acceptance: the x86-64 REALPATH loads both from their slots, as arm64 does. Depends: a8fb553d. Claim: unassigned.
