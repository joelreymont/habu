---
title: Read the pty echo to a marker, not to one read
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T11:35:59.954398+03:00\""
---

Problem: test/process-pty-tty-smoke.f types at a pseudo-terminal and fails about half the time on a loaded box with or without the gate's engine variables (whitebox lane, 2026-09-17: consecutive identical runs gave ok / failures), failing mid-echo expecting `?` after an 893-byte partial read; it was one of gate S's three reds at load 24 and passed three consecutive standalone runs at load 15, and LESSONS.md already records test/proc-pty.f case 10 as host-load sensitive. Acceptance: the smoke test reads until the expected marker or a deadline rather than assuming one read returns the whole echo (the shape lib/net/tcp4.f's deadline waits and test/proc-pty.f's poll loop use), so partial reads are not failures; the same for any other pty case that asserts on a single read; ten consecutive runs green at load above 15. Files: test/process-pty-tty-smoke.f, test/proc-pty.f, lib/process-pty*.f if the read helper belongs there. Verify: ten runs under load; test/run.f. Depends: none. Ownership: process/pty tests. Claim: agent=hazel-pty-marker workspace=.jj-ws/hazel-pty-marker.
