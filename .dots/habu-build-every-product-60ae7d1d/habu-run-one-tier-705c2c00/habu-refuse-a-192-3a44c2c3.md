---
title: Refuse a 192-cell locals frame at tier 1 with a diagnostic
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.818501+02:00"
closed-at: "2026-11-04T11:48:49.790381+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. Tier-1 uncaught -8575 at 1 set-tier; T0's large-frame run becomes a T0 test; see DIVERGENCES for three frame limits."
---

Problem: 22 locals of a 192-cell PRODUCT: tier 1 ends 'ncomp: cannot compile GE-WFRAME at GE-MK192', uncaught -8575 rc 67; T0 defines it (x16 fallback past the scaled reach, a 4224-cell frame never run); the JIT refuses 'locals frame over 4096 cells'. Repro /tmp/jerry-big/gaps/pass2-frame-*.f. Acceptance: tier 1 compiles or refuses by name; T0's large frame proven by a run. Claim: unassigned.
