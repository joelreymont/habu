---
title: "INCIDENT: master red - prop-test rejects schema-n@ candidate"
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T21:38:30.589665+02:00"
---

STOP-EVERYTHING (2026-08-20): test/prop-test.f fails on master b5e5cdb3 - 'primitive candidate rejected: : PROP-PC ( -- n ) schema-n@ ;' + 'primitive semantic case failed', checker complaint inside the generated word (at dup expected: i64 actual: i64 i64). Bisected by ir-1: passed at d98d1d7a (the schema seal), broken by 9f598292 (the NIMM deletion) - which touches no schema file, so the mechanism is candidate-pool shift: deleting NIMM's words moved prop-test's selection onto SCHEMA-N@, whose post-seal state (axiom relocated to PPRIM: SCHEMA-REG at d98d1d7a) fails the semantic leg on first contact. The seal may have left the bare spelling enumerable with a stale/mismatched effect row, exposed only when selection landed on it. SECOND DEFECT in the same incident: my merge gate ran test/run.f green on the exact red tree - prop-test is in test/run-files.f:97 but evidently not in the resident execution path; the wired-is-not-runs drift is now incident-grade. FIX BOTH: the schema-n@ effect mismatch at its root (why didn't the relocation carry the effect - or why is the bare spelling still enumerable), AND register prop-test in the path the gate actually executes. No merges until master green.

Claim: agent=incident-1 workspace=.jj-ws/habu-trusted

RCA (incident-1, re-verified endpoints, same bin/hb 48f5ea7f throughout):
the bisection above is wrong and both of its mechanisms are wrong. prop-test's
primitive list is a STATIC table, not an enumeration, so no candidate pool ever
shifted; and the "at dup expected: i64 actual: i64 i64" line is the shrink
self-test's own expected output, not part of this failure. Measured:
9117cffd (pre-seal) GREEN, 4370532e (the seal) RED, d98d1d7a RED, 9f598292 RED.
The seal broke it, not the NIMM deletion.

Defect 1. test/prop-test-core.f:693-694 named the two schema axioms by their
pre-seal bare spelling. The seal relocated them to SCHEMA-REG:SCHEMA-N@ /
SCHEMA-REG:SCHEMA-ROOT-N@ (checker.f PPRIM: SCHEMA-REG), so the bare spelling
is E-UNDEFINED and the checker refused the candidate - correctly. EFFECT-QUERY
confirms: no effect for schema-n@, an effect for schema-reg:schema-n@. The
case list is an inventory of the checker's axiom table and two rows were
renamed; it now names them the way the rest of the list already names package
axioms (type-field:tx-depth, lower-cert:magic).

Defect 2 is NOT a wiring gap. prop-test IS in the executed path:
test/gate-debug-lib.f GDB-PROP runs `bin/hb` with test/prop-test.f on stdin,
in phase 6, started first by TEST:DAG-RUN-REST. The gate was green on the red
tree because the phase was served from the result cache as "PASS (cached)".
Root: bin/hb bakes only primitives and re-reads its whole checker/core prefix
from the checkout at every boot, so a src/core edit moves a phase's verdict
while leaving the binary byte-identical - and the phase key named `bin/hb`
alone. Proved by recomputing the key: 9117cffd (GREEN) and 4370532e (RED) key
to the same c6725cb4..., and master's key 50124d20... matched a stamp written
21:13 by a run whose src/core was still pre-seal. Fix: the key now folds
test/run-engine-set.f ENGINE-SET:FILES = bin/hb plus tools/boot-pin.f BP-EACH,
the canonical boot-prefix list that test/boot-pin-test.f holds to habu2.f.
