---
title: test/ files end their waits by a clock, group 1
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.073703+01:00"
closed-at: "2026-11-13T21:29:13.935067+01:00"
---

Problem: test/aot-shifted-bytes.f, boot-relocation-e2e.f, code-reclaim.f, code-window.f, debugger-resume.f and five test/compiler/native-*.f files end waits by deadlines. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wT1.
