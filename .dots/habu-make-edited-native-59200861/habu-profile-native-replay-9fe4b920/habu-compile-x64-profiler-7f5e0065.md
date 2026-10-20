---
title: Compile x64 profiler rewind
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T12:53:45.247582+03:00"
closed-at: "2026-10-19T13:01:28.674352+03:00"
close-reason: Use the public MOV register emitter at rewind. Matching native cross-profiler test passes and builds all twelve ELF fixtures; independent Astra review clear. Linux execution was not claimed.
---

New matching-native cross profiler harness test/x86-64-kernel-prof.f fails before artifacts: kernel-x64.f:2798 names unqualified COPY, but that helper is private to X64PROF. Replace the unavailable helper with the existing equivalent target assembler operation, preserve count/stack behavior, and build all existing/new profiler ELF fixtures. This is source cross-emitter work, not a platform-port campaign.
