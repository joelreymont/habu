---
title: Define MIN-INT -1 / the same on every backend
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T20:02:48.713357+03:00"
---

Problem: 'MIN-INT -1 /' returns MIN-INT on the arm64 engine (sdiv wraps) while x86_64 idiv traps on that quotient (SIGFPE), so the parity gate leaves the case out (parity lane, 2026-09-17) and the two backends would disagree on a defined checked program. Acceptance: the contract chosen and written down (wrap to MIN-INT everywhere, or a named refusal everywhere, decided with habu-refuse-integer-division), the x86_64 bodies guarding idiv accordingly, the parity gate pinning the case, docs/forth.md stating it. Files: src/habu/habu1.f, the x86_64 primitive bodies, test/prim-parity.f, docs/forth.md. Verify: the parity gate on both backends. Depends: habu-refuse-integer-division (the zero-divisor dot), habu-cross-build-the-d25a959d. Ownership: engine primitives. Claim: unassigned.
