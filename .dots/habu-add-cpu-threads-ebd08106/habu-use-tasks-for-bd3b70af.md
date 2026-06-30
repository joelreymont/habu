---
title: Use tasks for PTX host async
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T14:56:04.721622+02:00"
closed-at: "2026-06-30T14:57:33.353934+02:00"
close-reason: not required for Odin port scope
---

Problem: tasking is not yet proven against the GPU workflow. It should not be part of core task correctness, but Habu's current focus is PTX/GPU, so tasks need an integration proof where they help host orchestration without touching CUDA context rules unsafely. Fix: after PTX C-ABI/Driver harness is green, add a zed-owned integration fixture that uses Habu tasks for host-side async compile/load/launch/copy or CPU golden work, with explicit CUDA context ownership rules and no shared mutable checker/compiler state while tasks are live. Files: lib/task.f, PTX/CUDA driver harness files, docs/threads.md, PTX docs. Verify: zed device fixture passes; macOS static/checker fixture proves API shape without CUDA runtime.
