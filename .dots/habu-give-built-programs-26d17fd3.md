---
title: Give built programs a DT_DEBUG entry
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T12:19:34.736695+02:00"
---

Problem: src/os/linux/elf.f:256 ELF-DYNAMIC, (and src/os/linux-x86-64/elf.f) writes no DT_DEBUG entry, so the dynamic linker publishes no link map a debugger can find: gdb -batch -ex 'break mallopt' -ex run on a built program that calls mallopt runs to the end without stopping. Acceptance: built programs carry DT_DEBUG, and that gdb command stops at mallopt. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/bob-dt-debug.
