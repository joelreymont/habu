---
title: PTY device layer for the process supervisor
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T11:36:18.534344+02:00"
---

The supervisor in lib/process-pty-io.f runs children over pipe I/O, which the campaign scope permitted as the first slice; interactive REPL supervision needs the real PTY device layer: opening the pseudo-terminal pair (Linux /dev/ptmx with the grant/unlock ioctls and pts naming; the macOS equivalents), wiring the slave as the child's stdio around exec, window-size and termios control, and the same linear handle registry discipline for the master fd. Per-OS code with per-OS tests, following the src/os proc-watch/proc-control emitter pattern if new primitives are needed (each a separate seed-affecting slice). The device-side test needs a live terminal-shaped fixture on both OSes; the existing test/proc-pty.f PTY helpers show the Linux idioms. Recommended by the supervisor slice review; the supervisor's SPAWN/LAUNCH/TEARDOWN lifecycle is designed to take the PTY fds through the same GROUP-WATCH/IO fd slots.
