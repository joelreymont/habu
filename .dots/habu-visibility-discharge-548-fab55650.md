---
title: "Visibility discharge: 548 shims are not a typing problem"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:19.371299+02:00"
---

Phase 1 of habu-trusted-dies-prim-4fd12d60, the highest-leverage cut: 548 of 1,373 TRUSTED: sites (census 2026-08-19) are single-call shims onto words that are ALREADY checked : definitions - the checker knows their effects; internal-mark.f seals the name or the pre-hook prefix never recorded a usig at the call site (internal-mark.f:26 states it). Includes 79 of the 107 sites in the decl machinery (enum-decl 41/44, structure-decl 38/40, generated-declaration 11/23, structure-make 6/8) and ~395 of test/'s 826. Build ONE capability: publish an already-known signature to a named consumer package (friend-import / signature re-export). PROBE FIRST what internal-mark/prot-wid/friend arenas already provide - the capability may be an extension, not a new mechanism. Then sweep the 548 mechanically. Blocks the final TRUSTED: deletion.

Claim: unassigned (RELEASED 2026-08-22: workspace gone, no live lane - the 2026-08-21 gc keyed on dot-id workspaces and missed the shared habu-thecut/habu-trusted names)

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

## Leaves (2026-10-01 plan; census in the parent)

Measured: the rows of FRESH, CHECK-CANDIDATE-START, SCHEMA-REG:SCHEMA-CON, LOWER-CERT:ARENAS-STALE, USIGS, XREF-REC exist (EFFECT-QUERY true on the product) but carry no external authority: this is an authority gap, not a recording gap. Whitebox `: X1 ( -- n ) FRESH ;` is refused E-CAP-TRUSTED; the product answers E-UNDEFINED (sealed DNAME-INT).
test, 654 non-evaluate sites: single-call shims 315, raw-memory 96, multi-token 73, empty body 49, stack-op 26, literal 13, catch 7, trust-decl 3, execute 3. By class: (d) 63, trust-decl/`[ ]` 5 (go with 42b30edd), (b) ~90, (c) ~300, multi-token whitebox readers ~195 (decided per site by the probe). 84 registered files probed (~/.cache/tmp/heron-arm64/trusted/), 51 unregistered fixtures not probed.

B8 (Fable design, then worker-max; lands after habu-share-reopen-name-92885254 and the seal c550102f): the whitebox image binds the real recorded row of an internal word, so a whitebox body is checked against the row instead of asserted. Candidate from the plan: IMK-PASS's IMAGE-WHITEBOX arm sets a CHECKER-EFFECT-AUTHORITY cell that ENFORCED? reads; product unchanged. To verify in design: the window fixtures re-include checker.f from source, so their pre-hook rows may be absent rather than non-external. Designed together with 41e973ce's B7 (src shims onto pre-hook registry words). Fixture: test/internal-word-gate.f keeps the product refusal; a whitebox fixture pins `: X ( -- n ) FRESH ;` certifying.
B9a-d (worker each, after B8): per site `TRUSTED:` -> `:`; a product SUITE file that then fails E-UNDEFINED moves to WHITEBOX-SUITE; E-MISMATCH sites are casts and wait for B10; (d) sites wait for B3/B5. 9a type-family/type-decl/type-ctor/type-family-rollback/type-export suites and checker-scan-index-lib.f with its suites (~230); 9b engine-suite.f, engine-writers.f, effect-intern-suite.f, effect-read-api-test.f, checker-effect-authority.f, checker-rollback-sig-pool.f, catch-stale-suite.f, checker-verify-order.f, decl-event-suite.f, decl-replay-verify-source.f (~170); 9c structure-decl/structure-make/enum-decl/structure-certify suites, layout-*.f, registry-persist.f, rigid-region-suite.f (~90); 9d the 13 window fixtures and the aot-payload/registry children (~80). Acceptance per file: its row green on its engine, T-REPORT counts unchanged, rg count equals the residual (d)+(b).
