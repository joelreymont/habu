---
title: "Resolve realpath's and free's addresses once"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T04:45:37.289276+02:00"
---

Problem: src/habu/habu1.f REALPATH (:2871) DLSYMs "free" and "realpath" on every call: a warm engine build resolves 4344 paths, 8688 lookups at 1.4 us each (12 ms). Acceptance: each address is looked up once per process; a warm engine build's realpath calls measured before and after. Depends: none. Claim: unassigned.
