---
title: Add spawn-fds to the engine
status: active
priority: 1
issue-type: task
created-at: "2026-10-28T20:27:02.206450+02:00"
---

Fork-to-exec lane 1a (census .jj-ws.noindex/carl-fork/.out/spawn-census.md). One engine primitive spawn-fds over a request record (path, argv, envp, cwd, fd map, map length) on arm64 macOS (raw posix_spawn: POSIX_SPAWN_CLOEXEC_DEFAULT, one dup2 action per slot, runtime-sized frame), arm64 Linux and x86-64 Linux (error pipe above the map length, close_range(len, ~0, CLOSE_RANGE_CLOEXEC), add syscall 436 to both Linux sys.f, dup2 loop, execve), keeping the four old primitives; primitive-registry COMPLETE needs every backend in one commit. Sources are first moved to fds at or above the map length so a swapped map such as [1,0] is not clobbered. Rebuild bin/hb after it. Claim: carl.
