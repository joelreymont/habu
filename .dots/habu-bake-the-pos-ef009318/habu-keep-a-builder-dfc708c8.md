---
title: Keep a builder address out of a part
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:53:39.731467+01:00"
---

Problem: a part can still bake an address of the build into a raw cell through `PTR>N`, a distance from `NULL-PTR`, or a private cast of `here`; the object copies the number and a warm build links a stale address. Needs a design. Acceptance: each of the three is rejected or carried as a reference, the same cold and warm. Depends: none. Claim: unassigned.
