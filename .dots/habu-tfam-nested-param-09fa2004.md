---
title: TFAM nested-param sig reference crashes checker
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T04:11:31.431900+02:00"
---

Referencing a STORED nested-param signature crashes the checker (SIGSEGV, ~80k-deep stack overflow, EXC_BAD_ACCESS write near stack guard) under accumulated checker state. Enabled by dot habu-tfam-4 (registry-driven nested family<...> parsing). No existing code triggers it (no nested params in lib/ptx); the full native gate is green without it.

REPRO (crashes only inside test/engine-suite.f accumulated state; passes standalone):
  s" COK-TFAM-NEST ( acc<t,tile<t,b,m>,b> -- acc<t,tile<t,b,m>,b> )" T-CHECK-PASSES
  s" COK-TFAM-NEST-CALL ( acc<t,tile<t,b,m>,b> -- acc<t,tile<t,b,m>,b> ) COK-TFAM-NEST" T-CHECK-PASSES   \ <-- crash
Standalone (fresh engine) the same two definitions via CHECK! succeed. A NON-nested param sig reference (engine-suite COK-PTX-ID-CALL -> COK-PTX-ID) works. So it is specific to instantiating/traversing a STORED NESTED T-PARAM via E-INST + unify.

FINDINGS: not E-INST recursion (depth guard at 300 never fired); not the unify worklist (U-PUSH dies at 4096, would print 'unify worklist full', not segfault); ~80k-deep recursion in an UNGUARDED recursive term/effect walker (candidates: E-COPY of the inferred effect, TY-OCC? occurs-check hard-unrolled to 4 args, LIN-TYPE-COUNT, or QREND) following a CYCLE created when the stored nested param is instantiated (E-INST) and unified against the declared output. Hypothesis: a fresh-var instantiation or arg-copy binds a T-PARAM arg to a spine containing itself, and a downstream walker follows it without a depth/occurs guard.

NEXT: reproduce minimally by replicating engine-suite pre-state (param-arena-grow shrink+restore at test/engine-suite.f:564-575) then the two defs; add a cycle/depth guard to E-COPY and TY-OCC? to localize; verify TY-OCC?/occurs covers nested T-PARAM args (>index 3 too once growable SoA lands). Re-add the reference + cross-shape regressions to engine-suite (marked with NOTE near CBAD-TFAM-PTRARITY) once fixed.
