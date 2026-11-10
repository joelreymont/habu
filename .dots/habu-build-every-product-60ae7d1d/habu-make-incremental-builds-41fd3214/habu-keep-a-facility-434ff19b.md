---
title: "Keep a facility taken during a build's load out of the object"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T07:02:03.715479+02:00"
---

Problem: Darwin's first lock of a PTHREAD_MUTEX_INITIALIZER mutex writes the bitwise NOT of the mutex's address at +48 (measured by the layout review, 2026-11-10: a static mutex at 0x102bc4000 holds fffffffefd43bfff after one lock and unlock; locking a moved copy still returns 0). hb-build runs a product's top-level code, so a load that pipes, spawns or opens a pty takes FD-LOCK (FD-LOCK:WITH callers in fd.f, spawn.f, process-fork.f, process.f, pty-harness.f, process-pty-io.f) and one that logs takes log.f's LOCK; a later file's object then carries a store of the builder's value into the lock, and nothing refuses it. No such call was found in hb's own build. Cost: two builds of such a program differ in bytes; no crash. Acceptance: a program whose load takes a facility builds to the same bytes cold and warm, or the build refuses the load naming the facility. Files: src/lib/task.f FACILITY, GET; src/habu/linker.f if the linker must drop the stores. Verify: a fixture whose top level calls LOG, built cold twice and warm, cmp. Depends: none. Ownership: carl. Claim: unassigned.
