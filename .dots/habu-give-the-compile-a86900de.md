---
title: Give the compile-scaling yardstick a control test
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T13:01:30.243191+03:00"
---

Problem: tools/compile-scaling.f (habu-make-register-allocation-d7ba9e30) claims a process may load it and carry on unchanged, the way tools/compile-floor.f does, but nothing holds that claim open: test/compiler/compile-floor.f covers the floor tool and a full scaling run is 14 s, too slow for a gate. Acceptance: a cheaper size ladder (or a --control mode) that exercises the borrow, both ratchet outcomes and the usage path in under two seconds, registered in test/gate-stdlib-cases.f, green on the root. Files: tools/compile-scaling.f, test/compiler/ (new case), test/gate-stdlib-cases.f. Verify: the new case standalone and through test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
