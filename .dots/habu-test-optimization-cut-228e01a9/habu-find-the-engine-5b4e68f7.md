---
title: Find the engine root where the kernel refuses its path
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T12:14:59.132737+02:00"
---

Problem (review 292 of 896d7a0b): under a sandbox that denies process-info-pidinfo, proc_pidpath refuses the engine its own path, so src/core/include.f ENGINE-ROOT-INIT (:324-336) gets the empty path and finds no tree root. A copied tree run from its parent (test/boot-relocation-e2e.f's layout) then re-reads the baked lib/errors.f as a foreign file and dies rc 78 'duplicate definition: E-A-FIRST at .../tree/lib/errors.f:16', which names a symptom, not the refused path (unsandboxed: rc 0). dyld's _NSGetExecutablePath answers under the same profile (probe: $HOME/.cache/tmp/kestrel-r4-rev292/nsget.c gives rc 0 and the absolute path while proc_pidpath fails with EPERM). Acceptance: on macOS the engine-root source (not ENGINE-ID:PATH$, whose kernel-reported meaning and KEY$ identity stay) falls back to _NSGetExecutablePath, canonicalized, when proc_pidpath refuses; the empty-path contract stays for a target with no such source (Linux without /proc). Shown by boot-relocation's layout under the profile: rc 0, failing first. Files: lib/engine-id.f or src/habu/native-runtime.f INSTALL-ENGINE-PATH, a sandboxed case beside test/boot-relocation-e2e.f. Baked: rebuild.
