---
title: Design allocation fault proof
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:16:01.349558+02:00"
---

Why: allocation-failure branches still need an honest production-path proof,
but release failure does not need an injector: a real misaligned mapping already
forces kernel rejection in an isolated child.

This parked dot owns only a future design for allocation-failure testing after
the inference critical path. Rejected commit `f08261db` is evidence, not a base:
its mutable production syscall defers add 484 lines and permanent indirection to
solve a test problem. This dot does not block fatal release, typed range unmap,
SAFET, WSTORE, or GPT-2 work.

Before activation, freeze one small test boundary that proves a real production
allocator caller's failure branch without an environment flag, mode flag,
value heuristic, public raw hook, mutable production defer, or silent fallback.
The contract must name the exact caller, owner, interface, and child-observable
failure before implementation. No claim is active.
