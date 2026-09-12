---
title: Run pre-trust-defer against a host that reads its prefix from source
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T12:48:02.937972+03:00"
---

Problem: test/pre-trust-defer.f copies src/ and lib/ into a temp root, patches src/core/exec-vector.f there and spawns bin/hb with cwd = that root, but the seeded product opens no prefix source at boot (strace: three openat calls, only the program itself), so every patch is inert: the positive case's child dies E-UNDEFINED: PTDX-POS and every negative case exits 0 where 72/70/76/73 was expected (12 cases red, measured 2026-09-12). Acceptance: the suite exercises the pre-trust defer backstop on an engine that reads the patched prefix: either the fixpoint's capture host (tools/build-fixpoint.f hb-host, built from source) is made available to the registry as the engine under test for this suite, or the suite drives the gforth-hosted stage0 (test/nf.fs) which reads source; the mechanism is chosen once and stated in the file header; the twelve cases are green or retired with the reason. Files: test/pre-trust-defer.f, test/gate-stdlib-cases.f, tools/build-fixpoint.f or test/nf.fs. Verify: the suite on bin/hb through the registry. Depends: habu-recover-stage0 (the recovery path). Ownership: hazel. Claim: unassigned.
