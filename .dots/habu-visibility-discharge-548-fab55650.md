---
title: "Visibility discharge: 548 shims are not a typing problem"
status: active
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:19.371299+02:00"
---

Phase 1 of habu-trusted-dies-prim-4fd12d60, the highest-leverage cut: 548 of 1,373 TRUSTED: sites (census 2026-08-19) are single-call shims onto words that are ALREADY checked : definitions - the checker knows their effects; internal-mark.f seals the name or the pre-hook prefix never recorded a usig at the call site (internal-mark.f:26 states it). Includes 79 of the 107 sites in the decl machinery (enum-decl 41/44, structure-decl 38/40, generated-declaration 11/23, structure-make 6/8) and ~395 of test/'s 826. Build ONE capability: publish an already-known signature to a named consumer package (friend-import / signature re-export). PROBE FIRST what internal-mark/prot-wid/friend arenas already provide - the capability may be an extension, not a new mechanism. Then sweep the 548 mechanically. Blocks the final TRUSTED: deletion.

Claim: agent=trusted-1 workspace=.jj-ws/habu-trusted

PREMISE FALSIFIED, RE-SCOPED (2026-08-19, trusted-1's measured probe through
install --force): of 595 single-call shim sites tree-wide, only 110 target a
name with a checker-recorded effect (50 targets, all PRIM-axiom'd); 485 target
names the checker holds NOTHING for, because the type foundation loads before
the check hook (habu2.f:861-890) - a RECORDING gap, not a seal gap.
internal-mark.f seals bare interpret/tick only; it never blocked a compiled
reference. Evidence: ED-PROBE-A (call SCHEMA-CON from enum-decl.f) dies rc 70
E-UNDEFINED at prefix time, before any seal exists; TRUSTED: at the owner does
not register either; EXPORT correctly refuses a no-effect source (7115).
LANDED: the 10 axiom-target forwarders (enum-decl 44->39, structure-decl
40->35), call sites bound to the checker's own rows - a wrong caller now dies
rc 70 (measured). REMAINING: ~100 mechanical sites (axiom-carrying targets,
MINUS deliberate retypes - a shim over @/drop/patch32/ffi-call restates a type
on purpose and must not be swept); 485 sites BLOCKED on route 3 (the
post-hook move - its own dot). RULING: route 1 (owner-side declared-signature
recorder) REJECTED - one-owner trust is still trust and route 3 deletes it;
route 2 (mass axioms) REJECTED - contradicts the epic. The instrument for any
future sweep: EFFECT-QUERY (checker.f:7015) + EFFECT-DIN-N/DOUT-N - ask the
checker, never classify by source shape.
