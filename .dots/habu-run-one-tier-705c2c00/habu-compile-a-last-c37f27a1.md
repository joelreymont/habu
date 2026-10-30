---
title: Compile a last-call recurse as a tail call in T0 and tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.793294+02:00"
---

Problem: T0 and tier 1 compile no last-call recurse as a tail call; GETS recursing 2000000 deep dies 'stack bounds exceeded (machine)' rc 102 where the JIT's last-call B (habu2.f EM-COMPILE-SEMI) runs. Repro /tmp/jerry-big/gaps/tail-*.f; test/runtime-regression-test.f GE-VTAIL-AT selects the JIT for its tier-0 leg until then. Acceptance: native tail call, GE-VTAIL-AT back on T0. Depends: T0 release. Claim: unassigned.
