---
title: Refuse a 192-cell locals frame at tier 1 with a diagnostic
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.818501+02:00"
---

Problem: 22 locals of a 192-cell PRODUCT: tier 1 ends 'ncomp: cannot compile GE-WFRAME at GE-MK192', uncaught -8575 rc 67; T0 defines it (x16 fallback past the scaled reach, a 4224-cell frame never run); the JIT refuses 'locals frame over 4096 cells'. Repro /tmp/jerry-big/gaps/pass2-frame-*.f. Acceptance: tier 1 compiles or refuses by name; T0's large frame proven by a run. Claim: unassigned.
