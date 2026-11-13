---
title: Bind free and realpath at load on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T10:13:14.477669+02:00"
---

Problem: src/habu/kernel-x64.f REALPATH (about :1016) looks up free and realpath by dlsym on every call, and src/os/linux-x86-64/elf.f (:219) writes only dlopen and dlsym imports; arm64 binds them at load (dot a8fb553d). Acceptance: the x86-64 writer reads the Linux import table and the x86-64 REALPATH loads both from their slots, as arm64 does. Depends: a8fb553d. Claim: unassigned.
