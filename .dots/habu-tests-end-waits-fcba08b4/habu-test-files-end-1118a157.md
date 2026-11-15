---
title: test/ files end their waits by a clock, group 3
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.095519+01:00"
closed-at: "2026-11-13T21:29:13.903965+01:00"
---

Problem: test/load-reject-diag-test.f, main-routing.f, memory-fault.f, name-length-test.f, proc-capture-signal.f, process-env-big-child.f, process-pty-tty-smoke.f, product-host-identity.f, prof-rewind.f and program-diagnostics-test.f end waits by deadlines. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wT3.
