---
title: Delete unconsumed policy and test-only mechanisms
status: closed
priority: 1
issue-type: task
created-at: "2026-08-24T01:27:41.013193+02:00"
closed-at: "2026-08-24T02:19:38.078067+02:00"
close-reason: "Landed d2b27b53: deleted eight unconsumed policy/test mechanisms, preserved production boundaries, and fixed the PTY readiness race exposed by the exact gate."
blocks:
  - habu-make-one-suite-2829bf78
---

Delete perf enforcement, scheduled-DDC marker, PRIM-LINK, source-arena headroom policy, copied orphan reaper, standalone boot pin, public-bin inventory, and the old require-cap ratchet; preserve their real production tests and direct tools.
