---
title: Log the OS faults os-cpu, entropy and power throw
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T11:14:59.808395+01:00"
---

Problem: docs/forth-card.md:585 says a failure caused outside the program is logged once, with its text, by the library that saw it, but src/lib/os-cpu.f (-9550, -9551), entropy.f (-9540) and power.f (-9640) throw their OS faults without logging them (found from Maki's osmem lane, 2026-11-19). Acceptance: each logs the failure once, with its text, before it throws. Depends: none. Claim: unassigned.
